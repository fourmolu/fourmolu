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
    hsSigTypeToType,
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
import Data.Maybe (fromMaybe)
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
    withHaddocks (Is #end) (Just str) $ do
      located t p_hsType
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

p_hsConDeclRecFields :: [LHsConDeclRecField GhcPs] -> R ()
p_hsConDeclRecFields xs =
  recordBraces $ sep commaDel (sitcc . located' p_hsConDeclRecField) xs

p_hsConDeclRecField :: HsConDeclRecField GhcPs -> R ()
p_hsConDeclRecField field@HsConDeclRecField {..} = withFieldHaddocks $ do
  sitcc $
    sep
      commaDel
      (located' (p_rdrName . foLabel))
      cdrf_names
  inci $ p_hsFun field
  where
    withFieldHaddocks action = do
      commaStyle <- getPrinterOpt poCommaStyle
      let doc = cdf_doc cdrf_spec
      when (commaStyle == Trailing) $
        mapM_ (p_hsDoc Pipe (With #endNewline)) doc
      action
      when (commaStyle == Leading) $
        mapM_ (inciByFrac (-1) . (newline >>) . p_hsDoc Caret (Without #endNewline)) doc

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
p_hsConDeclFieldWithDoc cdf = withHaddocks (Is #end) cdf.cdf_doc $ do
  p_hsConDeclField cdf

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
    L ann (HsFunTy _ multAnn l r) ->
      let (arg, doc) =
            case l of
              L _ (HsDocTy _ x doc_) -> (x, Just doc_)
              _ -> (l, Nothing)
       in ParsedFunArg
            { span = ann,
              arg,
              doc,
              multAnn,
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
  renderFunReprMult = p_hsType
  renderFunReprRet = p_hsType

----------------------------------------------------------------------------
-- FunRepr HsConDeclRecField

-- | FunRepr HsConDeclRecField renders a record field type annotation. If the
-- record field has a function type, we want to render it as a function
-- respecting `function-arrows`. For the most part, it behaves like HsTypeSig;
-- however, record fields can have additional syntax not in normal function
-- types, like specifying the multiplicity for the `::` or specifying
-- UNPACK/strictness, so we need to handle this specially.
instance FunRepr (HsConDeclRecField GhcPs) where
  parseFunRepr (L _ HsConDeclRecField {..}) =
    ParsedFunSig
      { sig = cdrf_spec.cdf_multiplicity,
        next = toRecFieldRepr False $ parseFunRepr cdrf_spec.cdf_type
      }
    where
      toRecFieldRepr :: Bool -> ParsedFunRepr (HsType GhcPs) -> ParsedFunRepr (HsConDeclRecField GhcPs)
      toRecFieldRepr seenArg = \case
        ParsedFunSig {} -> error "parseFunRepr @HsType unexpectedly returned ParsedFunSig"
        ParsedFunForall {..} ->
          ParsedFunForall
            { tele,
              next = toRecFieldRepr seenArg next
            }
        ParsedFunQuals {..} ->
          ParsedFunQuals
            { ctxs,
              next = toRecFieldRepr seenArg next
            }
        ParsedFunArg {..} ->
          ParsedFunArg
            { span,
              arg =
                let mUnpack =
                      if not seenArg
                        then Just cdrf_spec.cdf_unpack
                        else Nothing
                 in L (getLoc arg) (mUnpack, arg),
              doc,
              multAnn,
              next = toRecFieldRepr True next
            }
        ParsedFunReturn {..} ->
          ParsedFunReturn
            { ret =
                let mAnn =
                      if not seenArg
                        then Just (cdrf_spec.cdf_unpack, cdrf_spec.cdf_bang)
                        else Nothing
                 in L (getLoc ret) (mAnn, ret),
              doc
            }

  type FunReprSig (HsConDeclRecField GhcPs) = HsMultAnnOf (LocatedA (HsType GhcPs)) GhcPs
  renderFunReprSig multAnn = do
    space
    p_hsMultAnn (located' p_hsType) multAnn
    space
    token'dcolon

  type FunReprCtx (HsConDeclRecField GhcPs) = HsType GhcPs
  renderFunReprCtx = p_hsType

  -- Invariant: SrcUnpackedness should only be set for the first arg
  type FunReprArg (HsConDeclRecField GhcPs) = (Maybe SrcUnpackedness, LocatedA (HsType GhcPs))
  renderFunReprArg (mUnpacked, ty) =
    p_hsConDeclField
      CDF
        { cdf_ext = error "unused",
          cdf_unpack = fromMaybe NoSrcUnpack mUnpacked,
          cdf_bang = NoSrcStrict,
          cdf_multiplicity = error "unused",
          cdf_type = ty,
          cdf_doc = error "unused"
        }

  type FunReprMult (HsConDeclRecField GhcPs) = HsType GhcPs
  renderFunReprMult = p_hsType

  -- Invariant: SrcUnpackedness/SrcStrictness should only be set if there are
  -- no args
  type FunReprRet (HsConDeclRecField GhcPs) = (Maybe (SrcUnpackedness, SrcStrictness), LocatedA (HsType GhcPs))
  renderFunReprRet (mAnn, ty) =
    p_hsConDeclField
      CDF
        { cdf_ext = error "unused",
          cdf_unpack = unpack,
          cdf_bang = strictness,
          cdf_multiplicity = error "unused",
          cdf_type = ty,
          cdf_doc = error "unused"
        }
    where
      (unpack, strictness) = fromMaybe (NoSrcUnpack, NoSrcStrict) mAnn

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
