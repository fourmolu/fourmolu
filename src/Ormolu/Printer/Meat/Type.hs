{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    p_hsConDeclRecFields,
    p_hsConDeclField,
    p_hsConDeclFieldWithDoc,
    p_lhsTypeArg,
    p_hsSigType,
<<<<<<< HEAD
    hsOuterTyVarBndrsToHsType,
    hsSigTypeToType,
||||||| a1fd8b82
    p_hsForAllTelescope,
    hsOuterTyVarBndrsToHsType,
=======
    p_hsForAllTelescope,
    p_hsOuterTyVarBndrs,
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0
    lhsTypeToSigType,

    -- * Re-exports from Ormolu.Printer.Meat.Type.Function
    FunRepr (..),
    ParsedFunRepr,
    ParsedFunRepr' (..),
    p_hsFun,
  )
where

import Control.Monad
import Data.Choice (pattern Is, pattern With, pattern Without)
import GHC.Data.Strict qualified as Strict
import GHC.Hs hiding (isPromoted)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Var
import Ormolu.Config
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.OpTree (p_tyOpTree, tyOpTree)
import Ormolu.Printer.Meat.Declaration.StringLiteral
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsUntypedSplice)
import Ormolu.Printer.Meat.Type.Function
import Ormolu.Printer.Operators
import Ormolu.Utils
import Prelude hiding (span)

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
<<<<<<< HEAD
  ty@HsFunTy {} ->
    p_hsFun ty
||||||| a1fd8b82
  HsFunTy _ arrow x y -> do
    located x p_hsType
    space
    p_arrow (located' p_hsTypeR) arrow
    interArgBreak
    case unLoc y of
      HsFunTy {} -> p_hsTypeR (unLoc y)
      _ -> located y p_hsTypeR
=======
  HsFunTy _ multAnn x y -> do
    located x p_hsType
    space
    p_arrow (located' p_hsTypeR) multAnn

    interArgBreak
    case unLoc y of
      HsFunTy {} -> p_hsTypeR (unLoc y)
      _ -> located y p_hsTypeR
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0
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
<<<<<<< HEAD
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
    withHaddocks (Is #end) (Just str) $ do
      located t p_hsType
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
||||||| a1fd8b82
    p_hsDoc Pipe (With #endNewline) str
    located t p_hsType
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
=======
    p_hsDoc Pipe (With #endNewline) str
    located t p_hsType
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0
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
  XHsType ext -> case ext of
    HsCoreTy t -> atom @HsCoreTy t
    HsBangTy _ (HsSrcBang _ u s) t -> do
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
      p_hsConDeclRecFields fields
  where
    startsWithSingleQuote = \case
      HsAppTy _ (L _ f) _ -> startsWithSingleQuote f
      HsTyVar _ IsPromoted _ -> True
      HsExplicitTupleTy {} -> True
      HsExplicitListTy {} -> True
      HsTyLit _ HsCharTy {} -> True
      _ -> False

p_hsTypeAnnotation :: LHsType GhcPs -> R ()
p_hsTypeAnnotation ty =
  p_hsFunParsed @(HsType GhcPs) $
    ParsedFunSig
      { sig = (),
        next = parseFunRepr ty
      }

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

<<<<<<< HEAD
p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  recordBraces $ sep commaDel (sitcc . located' p_conDeclField) xs
||||||| a1fd8b82
p_hsContext' :: (HasLoc (Anno a)) => (a -> R ()) -> [XRec GhcPs a] -> R ()
p_hsContext' f = \case
  [] -> txt "()"
  [x] -> located x f
  xs -> parens N $ sep commaDel (sitcc . located' f) xs

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
      HsBndrKind _ k -> do
        space
        txt "::"
        breakpoint
        inci (located k p_hsType)
      HsBndrNoKind _ -> pure ()

data ForAllVisibility = ForAllInvis | ForAllVis

-- | Render several @forall@-ed variables.
p_forallBndrs ::
  (HasLoc l) =>
  ForAllVisibility ->
  (a -> R ()) ->
  [GenLocated l a] ->
  R ()
p_forallBndrs ForAllInvis _ [] = txt "forall."
p_forallBndrs ForAllVis _ [] = txt "forall ->"
p_forallBndrs vis p tyvars =
  switchLayout (locA <$> tyvars) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars
      case vis of
        ForAllInvis -> txt "."
        ForAllVis -> space >> txt "->"

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  braces N $ sep commaDel (sitcc . located' p_conDeclField) xs
=======
p_hsContext' :: (HasLoc (Anno a)) => (a -> R ()) -> [XRec GhcPs a] -> R ()
p_hsContext' f = \case
  [] -> txt "()"
  [x] -> located x f
  xs -> parens N $ sep commaDel (sitcc . located' f) xs

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
      HsBndrKind _ k -> do
        space
        txt "::"
        breakpoint
        inci (located k p_hsType)
      HsBndrNoKind _ -> pure ()

data ForAllVisibility = ForAllInvis | ForAllVis

-- | Render several @forall@-ed variables.
p_forallBndrs ::
  (HasLoc l) =>
  ForAllVisibility ->
  (a -> R ()) ->
  [GenLocated l a] ->
  R ()
p_forallBndrs ForAllInvis _ [] = txt "forall."
p_forallBndrs ForAllVis _ [] = txt "forall ->"
p_forallBndrs vis p tyvars =
  switchLayout (locA <$> tyvars) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars
      case vis of
        ForAllInvis -> txt "."
        ForAllVis -> space >> txt "->"

p_hsConDeclRecFields :: [LHsConDeclRecField GhcPs] -> R ()
p_hsConDeclRecFields xs =
  braces N $ sep commaDel (sitcc . located' p_hsConDeclRecField) xs
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0

<<<<<<< HEAD
p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = withFieldHaddocks $ do
||||||| a1fd8b82
p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  mapM_ (p_hsDoc Pipe (With #endNewline)) cd_fld_doc
=======
p_hsConDeclRecField :: HsConDeclRecField GhcPs -> R ()
p_hsConDeclRecField HsConDeclRecField {..} = do
  mapM_ (p_hsDoc Pipe (With #endNewline)) (cdf_doc cdrf_spec)
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0
  sitcc $
    sep
      commaDel
      (located' (p_rdrName . foLabel))
<<<<<<< HEAD
      cd_fld_names
  inci $ p_hsTypeAnnotation cd_fld_type
  where
    withFieldHaddocks action = do
      commaStyle <- getPrinterOpt poCommaStyle
      when (commaStyle == Trailing) $
        mapM_ (p_hsDoc Pipe (With #endNewline)) cd_fld_doc
      action
      when (commaStyle == Leading) $
        mapM_ (inciByFrac (-1) . (newline >>) . p_hsDoc Caret (Without #endNewline)) cd_fld_doc
||||||| a1fd8b82
      cd_fld_names
  space
  txt "::"
  breakpoint
  sitcc . inci $ p_hsType (unLoc cd_fld_type)
=======
      cdrf_names
  space
  p_hsMultAnn (located' p_hsType) (cdf_multiplicity cdrf_spec)
  space
  txt "::"
  breakpoint
  sitcc . inci $ p_hsConDeclField cdrf_spec

-- | This does not print 'cdf_doc' and 'cdf_multiplicity' as there is no single
-- strategy for where to print them (see call sites).
p_hsConDeclField :: HsConDeclField GhcPs -> R ()
p_hsConDeclField CDF {..} = do
  case cdf_unpack of
    SrcUnpack -> txt "{-# UNPACK #-}" *> space
    SrcNoUnpack -> txt "{-# NOUNPACK #-}" *> space
    NoSrcUnpack -> pure ()
  located cdf_type $ \ty -> do
    case cdf_bang of
      SrcLazy -> txt "~"
      SrcStrict -> txt "!"
      NoSrcStrict -> pure ()
    p_hsType ty

p_hsConDeclFieldWithDoc :: HsConDeclField GhcPs -> R ()
p_hsConDeclFieldWithDoc cdf = do
  mapM_ (p_hsDoc Pipe (With #endNewline)) (cdf_doc cdf)
  p_hsConDeclField cdf
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0

p_lhsTypeArg :: LHsTypeArg GhcPs -> R ()
p_lhsTypeArg = \case
  HsValArg NoExtField ty -> located ty p_hsType
  -- first argument is the SrcSpan of the @,
  -- but the @ always has to be directly before the type argument
  HsTypeArg _ ty -> txt "@" *> located ty p_hsType
  -- NOTE(amesgen) is this unreachable or just not implemented?
  HsArgPar _ -> notImplemented "HsArgPar"

p_hsSigType :: HsSigType GhcPs -> R ()
<<<<<<< HEAD
p_hsSigType = p_hsType . hsSigTypeToType
||||||| a1fd8b82
p_hsSigType HsSig {..} =
  p_hsType $ hsOuterTyVarBndrsToHsType sig_bndrs sig_body
=======
p_hsSigType HsSig {..} = do
  p_hsOuterTyVarBndrs sig_bndrs
  case sig_bndrs of
    HsOuterImplicit {} -> pure ()
    HsOuterExplicit {} -> breakpoint
  located sig_body p_hsType
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0

instance FunRepr (HsType GhcPs) where
  parseFunRepr = \case
    -- `forall a. _`
    L ann (HsForAllTy _ tele ty) ->
      ParsedFunForall
        { tele = L ann tele,
          next = parseFunRepr ty
        }
    -- `HasCallStack => _`
    ty@(L _ HsQualTy {}) ->
      let (ctxs, rest) = getContexts ty
       in ParsedFunQuals
            { ctxs,
              next = parseFunRepr rest
            }
    -- `Int -> _`
    L ann (HsFunTy _ arrow l r) ->
      let (arg, doc) =
            case l of
              L _ (HsDocTy _ x doc_) -> (x, Just doc_)
              _ -> (l, Nothing)
       in ParsedFunArg
            { span = ann,
              arg,
              doc,
              arrow,
              next = parseFunRepr r
            }
    -- `_ -> Int`
    L _ (HsDocTy _ ret doc) -> ParsedFunReturn {ret, doc = Just doc}
    ret -> ParsedFunReturn {ret, doc = Nothing}
    where
      getContexts =
        let go ctxs = \case
              L ann (HsQualTy _ ctx ty) ->
                go (L ann ctx : ctxs) ty
              ty ->
                (reverse ctxs, ty)
         in go []

  renderFunReprCtx = p_hsType
  renderFunReprArg = p_hsType
  renderFunReprArr = p_hsType
  renderFunReprRet = p_hsType

p_hsOuterTyVarBndrs ::
  HsOuterTyVarBndrs Specificity GhcPs ->
  R ()
p_hsOuterTyVarBndrs = \case
  HsOuterImplicit _ -> pure ()
  HsOuterExplicit _ bndrs -> p_hsForAllTelescope $ mkHsForAllInvisTele noAnn bndrs

----------------------------------------------------------------------------
-- Conversion functions

<<<<<<< HEAD
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

||||||| a1fd8b82
-- could be generalized to also handle () instead of Specificity
hsOuterTyVarBndrsToHsType ::
  HsOuterTyVarBndrs Specificity GhcPs ->
  LHsType GhcPs ->
  HsType GhcPs
hsOuterTyVarBndrsToHsType obndrs ty = case obndrs of
  HsOuterImplicit NoExtField -> unLoc ty
  HsOuterExplicit _ bndrs ->
    HsForAllTy NoExtField (mkHsForAllInvisTele noAnn bndrs) ty

=======
>>>>>>> refs/rewritten/Merge-ormolu-0-8-1-0
lhsTypeToSigType :: LHsType GhcPs -> LHsSigType GhcPs
lhsTypeToSigType ty =
  L (getLoc ty) . HsSig NoExtField (HsOuterImplicit NoExtField) $ ty
