{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Rendering of types.
module Ormolu.Printer.Meat.Type
  ( p_hsType,
    p_hsTypePostDoc,
    hasDocStrings,
    p_hsContext,
    p_hsTyVarBndr,
    ForAllVisibility (..),
    p_forallBndrs,
    p_conDeclFields,
    p_lhsTypeArg,
    p_hsSigType,
    tyVarsToTyPats,
    hsOuterTyVarBndrsToHsType,
    lhsTypeToSigType,
  )
where

import Control.Monad
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Functor (($>))
import GHC.Hs
import GHC.Types.Basic hiding (isPromoted)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Var
import Ormolu.Config
import Ormolu.Printer.Combinators
import Ormolu.Printer.Internal (enterLayout)
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.OpTree (p_tyOpTree, tyOpTree)
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsSplice, p_stringLit)
import Ormolu.Printer.Operators
import Ormolu.Utils

{-# ANN module ("Hlint: ignore Use camelCase" :: String) #-}

-- What precedes this type on the same line.
-- Only used for leading arrows
data After
  = AfterForall
  | AfterContext
  | AfterArgument
  | AfterNothing
  deriving (Eq, Show)

-- | Render an "After" infix
p_after :: Bool -> After -> R ()
p_after multilineArgs = \case
  AfterNothing -> pure ()
  AfterForall | multilineArgs -> txt " ." >> space
  AfterForall -> txt "." >> space
  AfterContext -> txt "=>" >> space
  AfterArgument -> txt "->" >> space

-- | like p_after but only when we're rendering leading arrows
p_leadingAfter :: Bool -> After -> R ()
p_leadingAfter multiline a = do
  getPrinterOpt poLeadingArrows >>= \case
    False -> pure ()
    True -> p_after multiline a

p_hsType :: HsType GhcPs -> R ()
p_hsType = p_hsTypeAfter' AfterNothing

-- | Like 'p_hsType' but indent properly following a forall
p_hsTypeAfter' :: After -> HsType GhcPs -> R ()
p_hsTypeAfter' after t = do
  s <- bool PipeStyle CaretStyle <$> getPrinterOpt poLeadingArrows
  layout <- getLayout
  p_hsType' after (hasDocStrings t || layout == MultiLine) s t

p_hsTypePostDoc :: HsType GhcPs -> R ()
p_hsTypePostDoc t = p_hsType' AfterNothing (hasDocStrings t) CaretStyle t

-- | How to render Haddocks associated with a type.
data TypeDocStyle
  = PipeStyle
  | CaretStyle

p_hsType' :: After -> Bool -> TypeDocStyle -> HsType GhcPs -> R ()
p_hsType' after multilineArgs docStyle =
  (p_leadingAfter multilineArgs after >>) . \case
    HsForAllTy _ tele t -> do
      a <- case tele of
        HsForAllInvis _ bndrs -> p_forallBndrs' ForAllInvis p_hsTyVarBndr bndrs
        HsForAllVis _ bndrs -> p_forallBndrs' ForAllVis p_hsTyVarBndr bndrs
      a' <-
        getPrinterOpt poLeadingArrows >>= \case
          True | multilineArgs -> pure a
          _ -> p_after False a $> AfterNothing
      interArgBreak
      p_hsTypeR a' (unLoc t)
    HsQualTy _ qs' t -> do
      getPrinterOpt poLeadingArrows >>= \case
        True -> do
          bool id inci3 (after == AfterForall) $ for_ qs' $ \qs -> do
            located qs p_hsContext
            interArgBreak
        False -> do
          for_ qs' $ \qs -> do
            located qs p_hsContext
            space
            txt "=>"
            interArgBreak
      case unLoc t of
        HsQualTy {} -> p_hsTypeR AfterContext (unLoc t)
        HsFunTy {} -> p_hsTypeR AfterContext (unLoc t)
        _ -> located t (p_hsTypeR AfterContext)
    HsTyVar _ p n -> do
      case p of
        IsPromoted -> do
          space
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
    HsFunTy _ arrow x y@(L _ y') -> do
      getPrinterOpt poLeadingArrows >>= \case
        True -> do
          bool id inci3 (after == AfterForall) (located x p_hsType)
          interArgBreak
        False -> do
          located x p_hsType
          space
          case arrow of
            HsUnrestrictedArrow _ -> txt "->"
            HsLinearArrow _ _ -> txt "%1 ->"
            HsExplicitMult _ _ mult -> do
              txt "%"
              p_hsTypeR AfterContext (unLoc mult)
              space
              txt "->"
          interArgBreak
      case y' of
        HsFunTy {} -> do
          layout <- getLayout
          -- Render the comments properly, but keep the existing layout
          located y (enterLayout layout . p_hsTypeR AfterArgument)
        _ -> located y (p_hsTypeR AfterArgument)
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
    HsOpTy _ x op y -> do
      fixityOverrides <- askFixityOverrides
      fixityMap <- askFixityMap
      let opTree = OpBranches [tyOpTree x, tyOpTree y] [op]
      p_tyOpTree
        (reassociateOpTree (Just . unLoc) fixityOverrides fixityMap opTree)
    HsParTy _ t ->
      parens N $ sitcc $ located t p_hsType
    HsIParamTy _ n t -> sitcc $ do
      located n atom
      trailingArrowType (pure ())
      breakpoint
      leadingArrowType (pure ())
      inci (located t p_hsType)
    HsStarTy _ _ -> txt "*"
    HsKindSig _ t k -> sitcc $ do
      located t p_hsType
      trailingArrowType (pure ())
      breakpoint
      leadingArrowType (pure ())
      inci (located k p_hsType)
    HsSpliceTy _ splice -> p_hsSplice splice
    HsDocTy _ t str ->
      case docStyle of
        PipeStyle -> do
          p_hsDocString Pipe True str
          located t p_hsType
        CaretStyle -> do
          located t p_hsType
          newline
          p_hsDocString Caret False str
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
      p_conDeclFields fields
    HsExplicitListTy _ p xs -> do
      case p of
        IsPromoted -> txt "'"
        NotPromoted -> return ()
      brackets N $ do
        -- If both this list itself and the first element is promoted,
        -- we need to put a space in between or it fails to parse.
        case (p, xs) of
          (IsPromoted, L _ t : _) | isPromoted t -> space
          _ -> return ()
        sep commaDel (sitcc . located' p_hsType) xs
    HsExplicitTupleTy _ xs -> do
      txt "'"
      parens N $ do
        case xs of
          L _ t : _ | isPromoted t -> space
          _ -> return ()
        sep commaDel (located' p_hsType) xs
    HsTyLit _ t ->
      case t of
        HsStrTy (SourceText s) _ -> p_stringLit s
        a -> atom a
    HsWildCardTy _ -> txt "_"
    XHsType t -> atom t
  where
    isPromoted = \case
      HsAppTy _ (L _ f) _ -> isPromoted f
      HsTyVar _ IsPromoted _ -> True
      HsExplicitTupleTy {} -> True
      HsExplicitListTy {} -> True
      _ -> False
    interArgBreak =
      if multilineArgs
        then newline
        else breakpoint
    p_hsTypeR a = p_hsType' a multilineArgs docStyle

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
p_hsContext = \case
  [] -> txt "()"
  [x] -> located x p_hsType
  xs -> parens N $ sep commaDel (sitcc . located' p_hsType) xs

class IsInferredTyVarBndr flag where
  isInferred :: flag -> Bool

instance IsInferredTyVarBndr () where
  isInferred () = False

instance IsInferredTyVarBndr Specificity where
  isInferred = \case
    InferredSpec -> True
    SpecifiedSpec -> False

p_hsTyVarBndr :: IsInferredTyVarBndr flag => HsTyVarBndr flag GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar _ flag x ->
    (if isInferred flag then braces N else id) $ p_rdrName x
  KindedTyVar _ flag l k -> (if isInferred flag then braces else parens) N . sitcc $ do
    located l atom
    trailingArrowType (pure ())
    breakpoint
    leadingArrowType (pure ())
    inci (located k p_hsType)

data ForAllVisibility = ForAllInvis | ForAllVis

-- | Render several @forall@-ed variables.
p_forallBndrs :: ForAllVisibility -> (a -> R ()) -> [LocatedA a] -> R ()
p_forallBndrs vis p tyvars = do
  a <- p_forallBndrs' vis p tyvars
  p_after False a

p_forallBndrs' :: ForAllVisibility -> (a -> R ()) -> [LocatedA a] -> R After
p_forallBndrs' ForAllInvis _ [] = txt "forall" $> AfterForall
p_forallBndrs' ForAllVis _ [] = txt "forall" >> space $> AfterArgument
p_forallBndrs' vis p tyvars = do
  switchLayout (getLocA <$> tyvars) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars
  case vis of
    ForAllInvis -> pure AfterForall
    ForAllVis -> space $> AfterArgument

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  braces N $ sep commaDel (sitcc . located' p_conDeclField) xs

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  commaStyle <- getPrinterOpt poCommaStyle
  when (commaStyle == Trailing) $
    mapM_ (p_hsDocString Pipe True) cd_fld_doc
  sitcc $
    sep
      commaDel
      (located' (p_rdrName . rdrNameFieldOcc))
      cd_fld_names
  getPrinterOpt poLeadingArrows >>= \case
    True -> sitcc . inci $ do
      space
      txt "::"
      space
      p_hsType (unLoc cd_fld_type)
    False -> do
      space
      txt "::"
      breakpoint
      sitcc . inci $ p_hsType (unLoc cd_fld_type)
  when (commaStyle == Leading) $
    mapM_ (inciByFrac (-1) . (newline >>) . p_hsDocString Caret False) cd_fld_doc

p_lhsTypeArg :: LHsTypeArg GhcPs -> R ()
p_lhsTypeArg = \case
  HsValArg ty -> located ty p_hsType
  -- first argument is the SrcSpan of the @,
  -- but the @ always has to be directly before the type argument
  HsTypeArg _ ty -> txt "@" *> located ty p_hsType
  -- NOTE(amesgen) is this unreachable or just not implemented?
  HsArgPar _ -> notImplemented "HsArgPar"

p_hsSigType :: HsSigType GhcPs -> R ()
p_hsSigType HsSig {..} =
  p_hsType $ hsOuterTyVarBndrsToHsType sig_bndrs sig_body

----------------------------------------------------------------------------
-- Conversion functions

tyVarToType :: HsTyVarBndr () GhcPs -> HsType GhcPs
tyVarToType = \case
  UserTyVar _ () tvar -> HsTyVar EpAnnNotUsed NotPromoted tvar
  KindedTyVar _ () tvar kind ->
    -- Note: we always add parentheses because for whatever reason GHC does
    -- not use HsParTy for left-hand sides of declarations. Please see
    -- <https://gitlab.haskell.org/ghc/ghc/issues/17404>. This is fine as
    -- long as 'tyVarToType' does not get applied to right-hand sides of
    -- declarations.
    HsParTy EpAnnNotUsed . noLocA $
      HsKindSig EpAnnNotUsed (noLocA (HsTyVar EpAnnNotUsed NotPromoted tvar)) kind

tyVarsToTyPats :: LHsQTyVars GhcPs -> HsTyPats GhcPs
tyVarsToTyPats HsQTvs {..} = HsValArg . fmap tyVarToType <$> hsq_explicit

-- could be generalized to also handle () instead of Specificity
hsOuterTyVarBndrsToHsType ::
  HsOuterTyVarBndrs Specificity GhcPs ->
  LHsType GhcPs ->
  HsType GhcPs
hsOuterTyVarBndrsToHsType obndrs ty = case obndrs of
  HsOuterImplicit NoExtField -> unLoc ty
  HsOuterExplicit _ bndrs ->
    HsForAllTy NoExtField (mkHsForAllInvisTele EpAnnNotUsed bndrs) ty

lhsTypeToSigType :: LHsType GhcPs -> LHsSigType GhcPs
lhsTypeToSigType ty =
  reLocA . L (getLocA ty) . HsSig NoExtField (HsOuterImplicit NoExtField) $ ty
