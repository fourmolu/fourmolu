{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module allows us to diff two 'ParseResult's.
module Ormolu.Diff.ParseResult
  ( ParseResultDiff (..),
    diffParseResult,
  )
where

import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Foldable
import Data.Function
import Data.Generics
import Data.List (sortOn)
import Data.Text qualified as T
import GHC.Data.FastString (FastString)
import GHC.Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Config.Gen (ImportGrouping (ImportGroupSingle))
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Utils
import Type.Reflection qualified as TR

-- | Result of comparing two 'ParseResult's.
data ParseResultDiff
  = -- | Two parse results are the same
    Same
  | -- | Two parse results differ
    Different [RealSrcSpan]
  deriving (Show)

instance Semigroup ParseResultDiff where
  Same <> a = a
  a <> Same = a
  Different xs <> Different ys = Different (xs ++ ys)

instance Monoid ParseResultDiff where
  mempty = Same

-- | Return 'Diff' of two 'ParseResult's.
diffParseResult ::
  ParseResult ->
  ParseResult ->
  ParseResultDiff
diffParseResult
  ParseResult
    { prCommentStream = cstream0,
      prParsedSource = hs0
    }
  ParseResult
    { prCommentStream = cstream1,
      prParsedSource = hs1
    } =
    diffCommentStream cstream0 cstream1
      <> diffHsModule
        hs0 {hsmodImports = concat . normalizeImports False mempty ImportGroupSingle $ hsmodImports hs0}
        hs1 {hsmodImports = concat . normalizeImports False mempty ImportGroupSingle $ hsmodImports hs1}

diffCommentStream :: CommentStream -> CommentStream -> ParseResultDiff
diffCommentStream (CommentStream cs) (CommentStream cs')
  | commentLines cs == commentLines cs' = Same
  | otherwise = Different []
  where
    commentLines = concatMap (toList . unComment . unLoc)

-- | Compare two modules for equality disregarding certain semantically
-- irrelevant features like exact print annotations.
diffHsModule :: HsModule GhcPs -> HsModule GhcPs -> ParseResultDiff
diffHsModule = genericQuery
  where
    genericQuery :: GenericQ (GenericQ ParseResultDiff)
    genericQuery x y
      -- 'ByteString' implements 'Data' instance manually and does not
      -- implement 'toConstr', so we have to deal with it in a special way.
      | Just x' <- cast x,
        Just y' <- cast y =
          if x' == (y' :: ByteString)
            then Same
            else Different []
      | Just rep <- isEpTokenish x,
        Just rep' <- isEpTokenish y =
          -- Only check whether the Ep(Uni)Tokens are of the same type; don't
          -- look at the actual payload (e.g. the location).
          if rep == rep' then Same else Different []
      | typeOf x == typeOf y,
        toConstr x == toConstr y =
          mconcat $
            gzipWithQ
              ( genericQuery
                  -- EPA-related
                  `extQ` considerEqual @SrcSpan
                  `ext1Q` epAnnEq
                  `extQ` considerEqual @SourceText
                  `extQ` considerEqual @EpAnnComments -- ~ XCGRHSs GhcPs
                  `extQ` considerEqual @EpaLocation
                  `extQ` considerEqual @EpLayout
                  `extQ` considerEqual @AnnSig
                  `extQ` considerEqual @HsRuleAnn
                  `extQ` considerEqual @EpLinearArrow
                  `extQ` considerEqual @AnnSynDecl
                  `extQ` considerEqual @IsUnicodeSyntax
                  -- FastString (for example for string literals)
                  `extQ` considerEqualVia' ((==) @FastString)
                  -- Haddock strings
                  `extQ` hsDocStringEq
                  -- Whether imports are pre- or post-qualified
                  `extQ` importDeclQualifiedStyleEq
                  -- Whether a class has an empty context
                  `extQ` classDeclCtxEq
                  -- Whether there are parens around a derived type class
                  `extQ` derivedTyClsEq
                  `extQ` typeEq
                  `extQ` dataDeclEq
                  `extQ` conDeclEq
                  -- For better error messages
                  `ext2Q` forLocated
              )
              x
              y
      | otherwise = Different []

    -- Return the 'TR.SomeTypeRep' of the type of the given value if it is an
    -- 'EpToken', an 'EpUniToken', or a list of these.
    isEpTokenish :: (Typeable a) => a -> Maybe TR.SomeTypeRep
    isEpTokenish = fmap TR.SomeTypeRep . go . TR.typeOf
      where
        go :: TR.TypeRep a -> Maybe (TR.TypeRep a)
        go rep = case rep of
          TR.App t t'
            | Just HRefl <- TR.eqTypeRep t (TR.typeRep @[]) ->
                TR.App t <$> go t'
          TR.App (TR.App t _) _ ->
            rep <$ TR.eqTypeRep t (TR.typeRep @EpUniToken)
          TR.App t _ ->
            rep <$ TR.eqTypeRep t (TR.typeRep @EpToken)
          _ -> Nothing

    considerEqualVia ::
      forall a.
      (Typeable a) =>
      (a -> a -> ParseResultDiff) ->
      a ->
      GenericQ ParseResultDiff
    considerEqualVia f x (cast -> Just x') = f x x'
    considerEqualVia _ _ _ = Different []

    considerEqualVia' f =
      considerEqualVia $ \x x' -> if f x x' then Same else Different []

    considerEqualOn ::
      (Typeable a, Data b) =>
      (a -> b) ->
      a ->
      GenericQ ParseResultDiff
    considerEqualOn f = considerEqualVia (genericQuery `on` f)

    considerEqual :: forall a. (Typeable a) => a -> GenericQ ParseResultDiff
    considerEqual = considerEqualVia $ \_ _ -> Same

    epAnnEq :: EpAnn a -> b -> ParseResultDiff
    epAnnEq _ _ = Same

    importDeclQualifiedStyleEq = considerEqualVia' f
      where
        f QualifiedPre QualifiedPost = True
        f QualifiedPost QualifiedPre = True
        f x x' = x == x'

    hsDocStringEq :: HsDocString -> GenericQ ParseResultDiff
    hsDocStringEq = considerEqualVia' ((==) `on` (map (T.dropWhile isSpace) . splitDocString True))

    forLocated ::
      (Data e0, Data e1) =>
      GenLocated e0 e1 ->
      GenericQ ParseResultDiff
    forLocated x@(L mspn _) y =
      maybe id appendSpan (cast `ext1Q` (Just . epAnnLoc) $ mspn) (genericQuery x y)
      where
        epAnnLoc :: EpAnn ann -> SrcSpan
        epAnnLoc = locA
    appendSpan :: SrcSpan -> ParseResultDiff -> ParseResultDiff
    appendSpan s' d@(Different ss) =
      case s' of
        RealSrcSpan s _ ->
          if not $ any (`isRealSubspanOf` s) ss
            then Different (s : ss)
            else d
        UnhelpfulSpan _ -> d
    appendSpan _ d = d

    -- The order of contexts doesn't matter
    normalizeContext :: HsContext GhcPs -> HsContext GhcPs
    normalizeContext = sortOn showOutputable

    normalizeMContext :: Maybe (LHsContext GhcPs) -> Maybe (LHsContext GhcPs)
    normalizeMContext Nothing = Nothing
    normalizeMContext (Just (L _ [])) = Nothing
    normalizeMContext (Just (L ann ctx)) = Just (L ann $ normalizeContext ctx)

    typeEq :: HsType GhcPs -> GenericQ ParseResultDiff
    typeEq = considerEqualOn $ \case
      HsQualTy ann ctx body -> HsQualTy ann (fmap normalizeContext ctx) body
      ty -> ty

    classDeclCtxEq :: TyClDecl GhcPs -> GenericQ ParseResultDiff
    classDeclCtxEq = considerEqualOn $ \case
      ClassDecl {..} -> ClassDecl {tcdCtxt = normalizeMContext tcdCtxt, ..}
      d -> d

    dataDeclEq :: HsDataDefn GhcPs -> GenericQ ParseResultDiff
    dataDeclEq = considerEqualOn $ \case
      HsDataDefn {..} ->
        HsDataDefn
          { -- The order of classes in the context doesn't matter
            dd_ctxt = normalizeMContext dd_ctxt,
            -- The order of deriving clauses doesn't matter. Note: need to normalize before sorting, otherwise
            -- we'll get a different sort order!
            dd_derivs = sortOn showOutputable ((fmap . fmap) normalizeDerivingClause dd_derivs),
            ..
          }

    conDeclEq :: ConDecl GhcPs -> GenericQ ParseResultDiff
    conDeclEq = considerEqualOn $ \case
      ConDeclGADT {..} -> ConDeclGADT {con_mb_cxt = normalizeMContext con_mb_cxt, ..}
      ConDeclH98 {..} -> ConDeclH98 {con_mb_cxt = normalizeMContext con_mb_cxt, ..}

    normalizeDerivingClause :: HsDerivingClause GhcPs -> HsDerivingClause GhcPs
    normalizeDerivingClause HsDerivingClause {deriv_clause_tys, ..} =
      HsDerivingClause {deriv_clause_tys = fmap normalizeDerivClauseTys deriv_clause_tys, ..}

    normalizeDerivClauseTys :: DerivClauseTys GhcPs -> DerivClauseTys GhcPs
    normalizeDerivClauseTys (DctSingle ann ty) = DctMulti ann [ty]
    -- The order of types in deriving clauses doesn't matter
    normalizeDerivClauseTys (DctMulti ann tys) = DctMulti ann (sortOn showOutputable tys)

    derivedTyClsEq :: DerivClauseTys GhcPs -> GenericQ ParseResultDiff
    derivedTyClsEq = considerEqualVia $ \lc rc -> genericQuery (normalizeDerivClauseTys lc) (normalizeDerivClauseTys rc)
