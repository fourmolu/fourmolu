{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeepSubsumption #-}
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
import Data.Foldable
import Data.Function
import Data.Generics
import GHC.Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Imports (noGroupingOperations, normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Utils

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
        hs0 {hsmodImports = concat . normalizeImports False noGroupingOperations $ hsmodImports hs0}
        hs1 {hsmodImports = concat . normalizeImports False noGroupingOperations $ hsmodImports hs1}

diffCommentStream :: CommentStream -> CommentStream -> ParseResultDiff
diffCommentStream (CommentStream cs) (CommentStream cs')
  | commentLines cs == commentLines cs' = Same
  | otherwise = Different []
  where
    commentLines = concatMap (toList . unComment . unLoc)

-- | Compare two modules for equality disregarding the following aspects:
--
--     * 'SrcSpan's
--     * ordering of import lists
--     * style (ASCII vs Unicode) of arrows, colons
--     * LayoutInfo (brace style) in extension fields
--     * Empty contexts in type classes
--     * Parens around derived type classes
--     * 'TokenLocation' (in 'LHsToken'/'LHsUniToken')
--     * 'EpaLocation'
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
      | typeOf x == typeOf y,
        toConstr x == toConstr y =
          mconcat $
            gzipWithQ
              ( genericQuery
                  `extQ` considerEqual @SrcSpan
                  `ext1Q` epAnnEq
                  `extQ` considerEqual @SourceText
                  `extQ` hsDocStringEq
                  `extQ` importDeclQualifiedStyleEq
                  `extQ` classDeclCtxEq
                  `extQ` derivedTyClsParensEq
                  `extQ` considerEqual @EpAnnComments -- ~ XCGRHSs GhcPs
                  `extQ` considerEqual @TokenLocation -- in LHs(Uni)Token
                  `extQ` considerEqual @EpaLocation
                  `extQ` considerEqual @EpLayout
                  `extQ` considerEqual @[AddEpAnn]
                  `extQ` considerEqual @AnnSig
                  `extQ` considerEqual @HsRuleAnn
                  `ext2Q` forLocated
                  -- unicode-related
                  `extQ` considerEqual @(EpUniToken "->" "→")
                  `extQ` considerEqual @(EpUniToken "::" "∷")
                  `extQ` considerEqual @EpLinearArrow
                  `extQ` considerEqualVia' compareAnnKeywordId
              )
              x
              y
      | otherwise = Different []

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
    hsDocStringEq = considerEqualVia' ((==) `on` splitDocString True)

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

    classDeclCtxEq :: TyClDecl GhcPs -> GenericQ ParseResultDiff
    classDeclCtxEq ClassDecl {tcdCtxt = Just (L _ []), ..} tc' = genericQuery ClassDecl {tcdCtxt = Nothing, ..} tc'
    classDeclCtxEq tc tc' = genericQuery tc tc'

    derivedTyClsParensEq :: DerivClauseTys GhcPs -> GenericQ ParseResultDiff
    derivedTyClsParensEq = considerEqualVia $ curry $ \case
      (DctSingle _ ty, DctMulti _ [ty']) -> genericQuery ty ty'
      (DctMulti _ [ty], DctSingle _ ty') -> genericQuery ty ty'
      (x, y) -> genericQuery x y

    compareAnnKeywordId x y =
      let go = curry $ \case
            (AnnCloseB, AnnCloseBU) -> True
            (AnnCloseQ, AnnCloseQU) -> True
            (AnnDarrow, AnnDarrowU) -> True
            (AnnDcolon, AnnDcolonU) -> True
            (AnnForall, AnnForallU) -> True
            (AnnLarrow, AnnLarrowU) -> True
            (AnnOpenB, AnnOpenBU) -> True
            (AnnOpenEQ, AnnOpenEQU) -> True
            (AnnRarrow, AnnRarrowU) -> True
            (Annlarrowtail, AnnlarrowtailU) -> True
            (Annrarrowtail, AnnrarrowtailU) -> True
            (AnnLarrowtail, AnnLarrowtailU) -> True
            (AnnRarrowtail, AnnRarrowtailU) -> True
            (_, _) -> False
       in go x y || go y x || x == y
