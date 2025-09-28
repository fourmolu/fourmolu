{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Rendering of types.
module Ormolu.Printer.Meat.Type
  ( p_hsType,
    p_hsTypeAnnotation,
    hasDocStrings,
    p_hsContext,
    p_hsContext',
    p_hsTyVarBndr,
    ForAllVisibility (..),
    p_forallBndrs,
    p_conDeclFields,
    p_lhsTypeArg,
    p_hsSigType,
    FunRepr (..),
    ParsedFunRepr (..),
    p_hsFun,
    hsOuterTyVarBndrsToHsType,
    hsSigTypeToType,
    lhsTypeToSigType,
  )
where

import Control.Monad
import Control.Monad.Cont qualified as Cont
import Control.Monad.State qualified as State
import Control.Monad.Trans qualified as Trans
import Data.Choice (Choice, pattern Is, pattern Isn't, pattern With, pattern Without)
import Data.Choice qualified as Choice
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (isJust)
import GHC.Data.Strict qualified as Strict
import GHC.Hs hiding (isPromoted)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Utils.Outputable (Outputable)
import Ormolu.Config
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.OpTree (p_tyOpTree, tyOpTree)
import Ormolu.Printer.Meat.Declaration.StringLiteral
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsUntypedSplice)
import Ormolu.Printer.Operators
import Ormolu.Utils

p_hsType :: HsType GhcPs -> R ()
p_hsType = \case
  ty@HsForAllTy {} ->
    p_hsFun ty
  ty@HsQualTy {} ->
    p_hsFun ty
  HsTyVar _ p n -> do
    case p of
      IsPromoted -> do
        txt "'"
        case showOutputable (unLoc n) of
          _ : '\'' : _ -> space
          _ -> return ()
      NotPromoted -> return ()
    p_rdrName n
  HsAppTy _ f x -> do
    let -- In order to format type applications with multiple parameters
        -- nicer, traverse the AST to gather the function and all the
        -- parameters together.
        gatherArgs f' knownArgs =
          case f' of
            L _ (HsAppTy _ l r) -> gatherArgs l (r : knownArgs)
            _ -> (f', knownArgs)
        (func, args) = gatherArgs f [x]
    switchLayout (getLocA f : fmap getLocA args) . sitcc $ do
      located func p_hsType
      breakpoint
      inci $
        sep breakpoint (located' p_hsType) args
  HsAppKindTy _ ty kd -> sitcc $ do
    -- The first argument is the location of the "@..." part. Not 100% sure,
    -- but I think we can ignore it as long as we use 'located' on both the
    -- type and the kind.
    located ty p_hsType
    breakpoint
    inci $ do
      txt "@"
      located kd p_hsType
  ty@HsFunTy {} ->
    p_hsFun ty
  HsListTy _ t ->
    located t (brackets N . p_hsType)
  HsTupleTy _ tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash N
            HsBoxedOrConstraintTuple -> parens N
     in parens' $ sep commaDel (sitcc . located' p_hsType) xs
  HsSumTy _ xs ->
    parensHash N $
      sep (space >> txt "|" >> breakpoint) (sitcc . located' p_hsType) xs
  HsOpTy _ _ x op y -> do
    modFixityMap <- askModuleFixityMap
    debug <- askDebug
    let opTree = BinaryOpBranches (tyOpTree x) op (tyOpTree y)
    p_tyOpTree
      (reassociateOpTree debug (Just . unLoc) modFixityMap opTree)
  HsParTy _ t -> do
    csSpans <-
      fmap (flip RealSrcSpan Strict.Nothing . getLoc) <$> getEnclosingComments
    switchLayout (locA t : csSpans) $
      parens N (sitcc $ located t p_hsType)
  HsIParamTy _ n t -> sitcc $ do
    located n atom
    inci $ p_hsTypeAnnotation t
  HsStarTy _ _ -> token'star
  HsKindSig _ t k -> sitcc $ do
    located t p_hsType
    inci $ p_hsTypeAnnotation k
  HsSpliceTy _ splice -> p_hsUntypedSplice DollarSplice splice
  HsDocTy _ t str -> do
    -- Usually handled by p_hsFun, but it's possible to have a bare type
    -- with docstrings, e.g.
    --
    --   type Name =
    --     -- | The name of a user as a string
    --     String
    --
    --   data User =
    --     User
    --       -- | Name
    --       String
    --       -- | Age
    --       Int
    usePipe <-
      getPrinterOpt poFunctionArrows <&> \case
        TrailingArrows -> True
        LeadingArrows -> False
        LeadingArgsArrows -> False
    if usePipe
      then do
        p_hsDoc Pipe (With #endNewline) str
        located t p_hsType
      else do
        located t p_hsType
        newline
        p_hsDoc Caret (Without #endNewline) str
  HsBangTy _ (HsBang u s) t -> do
    case u of
      SrcUnpack -> txt "{-# UNPACK #-}" >> space
      SrcNoUnpack -> txt "{-# NOUNPACK #-}" >> space
      NoSrcUnpack -> return ()
    case s of
      SrcLazy -> txt "~"
      SrcStrict -> txt "!"
      NoSrcStrict -> return ()
    located t p_hsType
  HsRecTy _ fields ->
    p_conDeclFields fields
  HsExplicitListTy _ p xs -> do
    case p of
      IsPromoted -> txt "'"
      NotPromoted -> return ()
    brackets N $ do
      -- If this list is promoted and the first element starts with a single
      -- quote, we need to put a space in between or it fails to parse.
      case (p, xs) of
        (IsPromoted, L _ t : _) | startsWithSingleQuote t -> space
        _ -> return ()
      sep commaDel (sitcc . located' p_hsType) xs
  HsExplicitTupleTy _ p xs -> do
    case p of
      IsPromoted -> txt "'"
      NotPromoted -> return ()
    parens N $ do
      -- If this tuple is promoted and the first element starts with a single
      -- quote, we need to put a space in between or it fails to parse.
      case (p, xs) of
        (IsPromoted, L _ t : _) | startsWithSingleQuote t -> space
        _ -> return ()
      sep commaDel (located' p_hsType) xs
  HsTyLit _ t ->
    case t of
      HsStrTy (SourceText s) _ -> p_stringLit s
      a -> atom a
  HsWildCardTy _ -> txt "_"
  XHsType t -> atom t
  where
    startsWithSingleQuote = \case
      HsAppTy _ (L _ f) _ -> startsWithSingleQuote f
      HsTyVar _ IsPromoted _ -> True
      HsExplicitTupleTy {} -> True
      HsExplicitListTy {} -> True
      HsTyLit _ HsCharTy {} -> True
      _ -> False

p_hsTypeAnnotation :: LHsType GhcPs -> R ()
p_hsTypeAnnotation = p_hsFunParsed . ParsedFunSig . parseFunRepr

-- | Return 'True' if at least one argument in 'HsType' has a doc string
-- attached to it.
hasDocStrings :: HsType GhcPs -> Bool
hasDocStrings = \case
  HsDocTy {} -> True
  HsFunTy _ _ (L _ x) (L _ y) -> hasDocStrings x || hasDocStrings y
  HsForAllTy _ _ (L _ x) -> hasDocStrings x
  HsQualTy _ _ (L _ x) -> hasDocStrings x
  _ -> False

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = p_hsContext' p_hsType

p_hsContext' ::
  (Outputable (GenLocated (Anno a) a), HasLoc (Anno a)) =>
  (a -> R ()) ->
  [XRec GhcPs a] ->
  R ()
p_hsContext' f = \case
  [] -> txt "()"
  [x] -> located x f
  xs -> do
    shouldSort <- getPrinterOpt poSortConstraints
    let sort = if shouldSort then sortOn showOutputable else id
    parens N $ sep commaDel (sitcc . located' f) (sort xs)

class IsTyVarBndrFlag flag where
  isInferred :: flag -> Bool
  p_tyVarBndrFlag :: flag -> R ()
  p_tyVarBndrFlag _ = pure ()

instance IsTyVarBndrFlag () where
  isInferred () = False

instance IsTyVarBndrFlag Specificity where
  isInferred = \case
    InferredSpec -> True
    SpecifiedSpec -> False

instance IsTyVarBndrFlag (HsBndrVis GhcPs) where
  isInferred _ = False
  p_tyVarBndrFlag = \case
    HsBndrRequired NoExtField -> pure ()
    HsBndrInvisible _ -> txt "@"

p_hsTyVarBndr :: (IsTyVarBndrFlag flag) => HsTyVarBndr flag GhcPs -> R ()
p_hsTyVarBndr HsTvb {..} = do
  p_tyVarBndrFlag tvb_flag
  let wrap
        | isInferred tvb_flag = braces N
        | otherwise = case tvb_kind of
            HsBndrKind {} -> parens N
            HsBndrNoKind {} -> id
  wrap $ do
    case tvb_var of
      HsBndrVar _ x -> p_rdrName x
      HsBndrWildCard _ -> txt "_"
    case tvb_kind of
      HsBndrKind _ k -> inci $ p_hsTypeAnnotation k
      HsBndrNoKind _ -> pure ()

data ForAllVisibility = ForAllInvis | ForAllVis

-- | Render several @forall@-ed variables.
p_forallBndrs ::
  (HasLoc l) =>
  ForAllVisibility ->
  (a -> R ()) ->
  [GenLocated l a] ->
  R ()
p_forallBndrs vis p tyvars = do
  p_forallBndrsStart p tyvars
  p_forallBndrsEnd (Without #extraSpace) vis

p_forallBndrsStart :: (HasLoc l) => (a -> R ()) -> [GenLocated l a] -> R ()
p_forallBndrsStart _ [] = token'forall
p_forallBndrsStart p tyvars = do
  switchLayout (locA <$> tyvars) $ do
    token'forall
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars

p_forallBndrsEnd :: Choice "extraSpace" -> ForAllVisibility -> R ()
p_forallBndrsEnd extraSpace = \case
  ForAllInvis -> txt dot >> space
  ForAllVis -> space >> token'rarrow
  where
    dot = if Choice.isTrue extraSpace then " ." else "."

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  recordBraces $ sep commaDel (sitcc . located' p_conDeclField) xs

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  commaStyle <- getPrinterOpt poCommaStyle
  when (commaStyle == Trailing) $
    mapM_ (p_hsDoc Pipe (With #endNewline)) cd_fld_doc
  sitcc $
    sep
      commaDel
      (located' (p_rdrName . foLabel))
      cd_fld_names
  inci $ p_hsTypeAnnotation cd_fld_type
  when (commaStyle == Leading) $
    mapM_ (inciByFrac (-1) . (newline >>) . p_hsDoc Caret (Without #endNewline)) cd_fld_doc

p_lhsTypeArg :: LHsTypeArg GhcPs -> R ()
p_lhsTypeArg = \case
  HsValArg NoExtField ty -> located ty p_hsType
  -- first argument is the SrcSpan of the @,
  -- but the @ always has to be directly before the type argument
  HsTypeArg _ ty -> txt "@" *> located ty p_hsType
  -- NOTE(amesgen) is this unreachable or just not implemented?
  HsArgPar _ -> notImplemented "HsArgPar"

p_hsSigType :: HsSigType GhcPs -> R ()
p_hsSigType = p_hsType . hsSigTypeToType

----------------------------------------------------------------------------
-- Rendering function types

-- | The parsed representation of a function
data ParsedFunRepr a
  = ParsedFunSig (ParsedFunRepr a)
  | ParsedFunForall
      (LocatedA (HsForAllTelescope GhcPs))
      (ParsedFunRepr a)
  | ParsedFunQuals
      [LocatedA (LocatedC [LocatedA a])]
      (ParsedFunRepr a)
  | -- | The argument, its optional docstring, and the arrow going to the next arg/return
    ParsedFunArgs
      [ LocatedA
          ( LocatedA a,
            Maybe (LHsDoc GhcPs),
            HsArrowOf (LocatedA a) GhcPs
          )
      ]
      (ParsedFunRepr a)
  | ParsedFunReturn
      ( LocatedA a,
        Maybe (LHsDoc GhcPs)
      )

class (Anno a ~ SrcSpanAnnA, Outputable a) => FunRepr a where
  renderFunItem :: a -> R ()

  parseFunRepr :: LocatedA a -> ParsedFunRepr a

instance FunRepr (HsType GhcPs) where
  renderFunItem = p_hsType
  parseFunRepr = \case
    -- `forall a. _`
    L ann (HsForAllTy _ tele ty) ->
      ParsedFunForall (L ann tele) (parseFunRepr ty)
    -- `HasCallStack => _`
    ty@(L _ HsQualTy {}) ->
      let (ctxs, rest) = getContexts ty
       in ParsedFunQuals ctxs (parseFunRepr rest)
    -- `Int -> _`
    ty@(L _ HsFunTy {}) ->
      let (args, ret) = getArgsAndReturn ty
       in ParsedFunArgs args (parseFunRepr ret)
    -- `_ -> Int`
    L _ (HsDocTy _ ty doc) -> ParsedFunReturn (ty, Just doc)
    ty -> ParsedFunReturn (ty, Nothing)
    where
      getContexts =
        let go ctxs = \case
              L ann (HsQualTy _ ctx ty) ->
                go (L ann ctx : ctxs) ty
              ty ->
                (reverse ctxs, ty)
         in go []
      getArgsAndReturn =
        let go args = \case
              L ann (HsFunTy _ arrow (L _ (HsDocTy _ l doc)) r) ->
                go (L ann (l, Just doc, arrow) : args) r
              L ann (HsFunTy _ arrow l r) ->
                go (L ann (l, Nothing, arrow) : args) r
              ty ->
                (reverse args, ty)
         in go []

-- | For implementing function-arrows and related configuration, we'll collect
-- all the components of the function type first, then render as a block instead
-- of rendering each part independently, which will let us track local state
-- within a function type.
--
-- This function should be passed the first function-related construct we find;
-- see FunRepr for more details.
p_hsFun :: (FunRepr a) => a -> R ()
p_hsFun = p_hsFunParsed . parseFunRepr . L (noAnn @SrcSpanAnnA)

p_hsFunParsed :: (FunRepr a) => ParsedFunRepr a -> R ()
p_hsFunParsed fun = do
  arrowsStyle <- getPrinterOpt poFunctionArrows
  p_hsFunParsed' arrowsStyle fun

type PrintHsFun x = State.StateT PrintHsFunState (Cont.ContT () R) x

type PrintHsFunState =
  ( Maybe (R ()), -- The leading delimiter to output at the next line
    Choice "forceMultiline"
  )

p_hsFunParsed' ::
  (FunRepr a) =>
  FunctionArrowsStyle ->
  ParsedFunRepr a ->
  R ()
p_hsFunParsed' arrowsStyle fun0 = Cont.evalContT . (`State.evalStateT` initialState) . go $ fun0
  where
    go = \case
      ParsedFunSig fun' -> do
        -- Should only happen at the very beginning, if at all
        p_parsedFunSig
        setMultilineContext fun'
        go fun'
      ParsedFunForall tele fun' -> do
        p_parsedFunForall tele
        setMultilineContext fun'
        go fun'
      ParsedFunQuals ctxs fun' -> do
        p_parsedFunQuals ctxs
        setMultilineContext fun'
        go fun'
      ParsedFunArgs args fun' -> do
        p_parsedFunArgs args
        go fun'
      ParsedFunReturn ret -> do
        p_parsedFunReturn ret

    liftR = Trans.lift . Trans.lift
    initialState = (Nothing, Without #forceMultiline)

    withApplyLeadingDelim f = do
      (leadingDelim, _) <- State.get
      a <- f leadingDelim
      State.modify $ \(_, x) -> (Nothing, x)
      pure a
    applyLeadingDelim = withApplyLeadingDelim (traverse_ liftR)

    setLeadingDelim delim =
      State.modify $ \(_, x) -> (Just delim, x)

    getIsMultiline = do
      (_, forceMultiline) <- State.get
      layout <- liftR getLayout
      pure $ Choice.toBool forceMultiline || layout == MultiLine

    setMultilineContext fun =
      State.modify $ \(x, _) -> (x, Choice.fromBool $ hasDocs fun)

    -- Make everything afterwards occur within a located block, with the
    -- magic of ContT. `item <- setLocated litem; ...` is equivalent to
    -- `located litem $ \item -> ...`.
    setLocated :: (HasLoc ann) => GenLocated ann x -> PrintHsFun x
    setLocated litem = Trans.lift $ Cont.ContT (located litem)

    p_parsedFunSig = do
      if isTrailing (Isn't #argDelim)
        then liftR $ do
          space >> token'dcolon
          if hasDocs fun0 then newline else breakpoint
        else do
          liftR breakpoint
          setLeadingDelim (token'dcolon >> space)

    p_parsedFunForall x@(L _ tele) = do
      void $ setLocated x
      applyLeadingDelim
      vis <-
        case tele of
          HsForAllInvis _ bndrs -> do
            liftR $ p_forallBndrsStart p_hsTyVarBndr bndrs
            pure ForAllInvis
          HsForAllVis _ bndrs -> do
            liftR $ p_forallBndrsStart p_hsTyVarBndr bndrs
            pure ForAllVis
      isMultiline <- getIsMultiline
      if isTrailing (Isn't #argDelim) || not isMultiline
        then do
          liftR $ p_forallBndrsEnd (Without #extraSpace) vis
          interArgBreak
        else do
          interArgBreak
          let extraSpace = if isMultiline then With #extraSpace else Without #extraSpace
          setLeadingDelim (p_forallBndrsEnd extraSpace vis)

    p_parsedFunQuals ctxs = do
      -- we only want to set located on the first context
      case ctxs of
        ctx : _ -> void $ setLocated ctx
        _ -> pure ()

      forM_ ctxs $ \(L _ lctx) -> do
        applyLeadingDelim
        liftR $ located lctx (p_hsContext' renderFunItem)
        if isTrailing (Isn't #argDelim)
          then do
            liftR $ space >> token'darrow
            interArgBreak
          else do
            interArgBreak
            setLeadingDelim (token'darrow >> space)

    p_parsedFunArgs args = do
      -- we only want to set located on the first arg
      case args of
        arg : _ -> void $ setLocated arg
        _ -> pure ()

      forM_ args $ \(L _ (larg, doc, arrow)) -> do
        let renderArrow = p_arrow (located' renderFunItem) arrow
        withHaddocks (Isn't #end) doc $ do
          withApplyLeadingDelim $ \applyLeadingDelim' ->
            liftR . located larg $ \arg -> do
              traverse_ id applyLeadingDelim'
              renderFunItem arg
          if isTrailing (Is #argDelim)
            then do
              liftR $ space >> renderArrow
              interArgBreak
            else do
              interArgBreak
              setLeadingDelim (renderArrow >> space)

    p_parsedFunReturn (ret, doc) = do
      void $ setLocated ret
      withHaddocks (Is #end) doc $ do
        applyLeadingDelim
        liftR $ located ret renderFunItem

    withHaddocks isEnd doc m = do
      isLeadingHaddock <-
        liftR $
          getPrinterOpt poHaddockLocSignature <&> \case
            HaddockLocSigAuto ->
              if isTrailing (Is #argDelim) then With #pipe else Without #pipe
            HaddockLocSigLeading ->
              With #pipe
            HaddockLocSigTrailing ->
              Without #pipe

      if Choice.isTrue isLeadingHaddock
        then do
          traverse (liftR . p_hsDoc Pipe (With #endNewline)) doc *> m
        else do
          let (pre, endNewline) =
                if Choice.isTrue isEnd
                  then (when (isJust doc) (liftR newline), Without #endNewline)
                  else (pure (), With #endNewline)
          m <* pre <* traverse (liftR . p_hsDoc Caret endNewline) doc

    interArgBreak = do
      isMultiline <- getIsMultiline
      liftR $ if isMultiline then newline else breakpoint

    hasDocs = \case
      ParsedFunSig fun -> hasDocs fun
      ParsedFunForall _ fun -> hasDocs fun
      ParsedFunQuals _ fun -> hasDocs fun
      ParsedFunArgs args fun -> or [isJust doc | L _ (_, doc, _) <- args] || hasDocs fun
      ParsedFunReturn (_, doc) -> isJust doc

    isTrailing isArgDelim =
      case arrowsStyle of
        TrailingArrows -> True
        LeadingArgsArrows -> not $ Choice.isTrue isArgDelim
        LeadingArrows -> False

----------------------------------------------------------------------------
-- Conversion functions

hsSigTypeToType :: HsSigType GhcPs -> HsType GhcPs
hsSigTypeToType HsSig {..} = hsOuterTyVarBndrsToHsType sig_bndrs sig_body

-- could be generalized to also handle () instead of Specificity
hsOuterTyVarBndrsToHsType ::
  HsOuterTyVarBndrs Specificity GhcPs ->
  LHsType GhcPs ->
  HsType GhcPs
hsOuterTyVarBndrsToHsType obndrs ty = case obndrs of
  HsOuterImplicit NoExtField -> unLoc ty
  HsOuterExplicit _ bndrs ->
    HsForAllTy NoExtField (mkHsForAllInvisTele noAnn bndrs) ty

lhsTypeToSigType :: LHsType GhcPs -> LHsSigType GhcPs
lhsTypeToSigType ty =
  L (getLoc ty) . HsSig NoExtField (HsOuterImplicit NoExtField) $ ty
