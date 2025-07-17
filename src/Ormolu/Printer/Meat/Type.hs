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
    p_hsForAllTelescope,
    p_hsQualArrow,
    p_hsFun,
    hsOuterTyVarBndrsToHsType,
    hsSigTypeToType,
    lhsTypeToSigType,
  )
where

import Control.Monad
import Data.Choice (Choice, pattern With, pattern Without)
import Data.Choice qualified as Choice
import Data.Functor ((<&>))
import Data.List (sortOn)
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
p_hsType t = do
  layout <- getLayout
  p_hsType' (Choice.fromBool $ hasDocStrings t || layout == MultiLine) t

p_hsType' :: Choice "multiline" -> HsType GhcPs -> R ()
p_hsType' isMultiline = \case
  HsForAllTy _ tele t -> do
    p_hsForAllTelescope isMultiline tele
    located t p_hsType
  HsQualTy _ qs t -> do
    located qs p_hsContext
    p_hsQualArrow isMultiline
    case unLoc t of
      HsQualTy {} -> p_hsTypeR (unLoc t)
      HsFunTy {} -> located t p_hsType
      _ -> located t p_hsTypeR
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
  HsFunTy _ arrow x y -> do
    located x p_hsType
    p_hsFun isMultiline p_hsTypeR arrow y
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
    p_hsTypeR m = p_hsType' isMultiline m

p_hsTypeAnnotation :: LHsType GhcPs -> R ()
p_hsTypeAnnotation lItem =
  getPrinterOpt poFunctionArrows >>= \case
    TrailingArrows -> do
      space
      token'dcolon
      breakTrailing
      located lItem p_hsType
    LeadingArrows -> do
      breakLeading
      located lItem $ \item -> do
        token'dcolon
        space
        p_hsType item
    LeadingArgsArrows -> do
      space
      token'dcolon
      breakTrailing
      located lItem p_hsType
  where
    breakTrailing =
      if hasDocStrings $ unLoc lItem
        then newline
        else breakpoint
    breakLeading = breakpoint

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
  p_forallBndrsEnd vis

p_forallBndrsStart :: (HasLoc l) => (a -> R ()) -> [GenLocated l a] -> R ()
p_forallBndrsStart _ [] = token'forall
p_forallBndrsStart p tyvars = do
  switchLayout (locA <$> tyvars) $ do
    token'forall
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars

p_forallBndrsEnd :: ForAllVisibility -> R ()
p_forallBndrsEnd ForAllInvis = txt "." >> space
p_forallBndrsEnd ForAllVis = space >> token'rarrow

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  braces N $ sep commaDel (sitcc . located' p_conDeclField) xs

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
  getPrinterOpt poFunctionArrows >>= \case
    LeadingArrows -> inci $ do
      breakpoint
      token'dcolon
      space
      p_hsType (unLoc cd_fld_type)
    TrailingArrows -> do
      space
      token'dcolon
      breakpoint
      sitcc . inci $ p_hsType (unLoc cd_fld_type)
    LeadingArgsArrows -> do
      space
      token'dcolon
      breakpoint
      sitcc . inci $ p_hsType (unLoc cd_fld_type)
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

p_hsForAllTelescope ::
  Choice "multiline" ->
  HsForAllTelescope GhcPs ->
  R ()
p_hsForAllTelescope isMultiline tele = do
  vis <-
    case tele of
      HsForAllInvis _ bndrs -> do
        p_forallBndrsStart p_hsTyVarBndr bndrs
        pure ForAllInvis
      HsForAllVis _ bndrs -> do
        p_forallBndrsStart p_hsTyVarBndr bndrs
        pure ForAllVis

  getPrinterOpt poFunctionArrows >>= \case
    LeadingArrows | Choice.isTrue isMultiline -> do
      interArgBreak
      txt " "
      p_forallBndrsEnd vis
    _ -> do
      p_forallBndrsEnd vis
      interArgBreak
  where
    interArgBreak = if Choice.isTrue isMultiline then newline else breakpoint

p_hsQualArrow :: Choice "multiline" -> R ()
p_hsQualArrow isMultiline =
  getPrinterOpt poFunctionArrows >>= \case
    LeadingArrows -> interArgBreak >> token'darrow >> space
    TrailingArrows -> space >> token'darrow >> interArgBreak
    LeadingArgsArrows -> space >> token'darrow >> interArgBreak
  where
    interArgBreak = if Choice.isTrue isMultiline then newline else breakpoint

p_hsFun ::
  (HasLoc l) =>
  Choice "multiline" ->
  (a -> R ()) ->
  HsArrowOf (GenLocated l a) GhcPs ->
  GenLocated l a ->
  R ()
p_hsFun isMultiline renderItem arrow rhsLoc =
  getPrinterOpt poFunctionArrows >>= \case
    LeadingArrows -> do
      interArgBreak
      located rhsLoc $ \rhs -> do
        renderArrow
        space
        renderItem rhs
    TrailingArrows -> do
      space
      renderArrow
      interArgBreak
      located rhsLoc $ \rhs -> do
        renderItem rhs
    LeadingArgsArrows -> do
      interArgBreak
      located rhsLoc $ \rhs -> do
        renderArrow
        space
        renderItem rhs
  where
    interArgBreak = if Choice.isTrue isMultiline then newline else breakpoint
    renderArrow = p_arrow (located' renderItem) arrow

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
