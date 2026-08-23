{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Ormolu.Imports.Grouping
  ( Import (..),
    ImportList (..),
    GroupImportsOpts (..),
    prepareExistingGroups,
    groupImports,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, minimumBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.ModuleName qualified as Cabal
import GHC.Hs (GhcPs, LEpaComment, epaLocationRealSrcSpan, getLocA)
import GHC.Types.SrcLoc (getLoc, srcSpanEndLine, srcSpanStartLine, srcSpanToRealSrcSpan)
import Language.Haskell.Syntax (LImportDecl, ModuleName, moduleNameString)
import Ormolu.Config (ImportGroup (..), ImportGroupRule (..), ImportGrouping (..))
import Ormolu.Config qualified as Config
import Ormolu.Utils (ghcModuleNameToCabal, groupBy')
import Ormolu.Utils.Glob (matchAllGlob, matchesGlob)

newtype ImportGroups = ImportGroups (NonEmpty ImportGroup)

data Import = Import
  { importName :: ModuleName,
    importList :: Maybe ImportList,
    importQualified :: Bool
  }

data ImportList
  = ImportList
  | HidingList
  deriving (Eq)

importGroupSingleStrategy :: ImportGroups
importGroupSingleStrategy =
  ImportGroups $
    pure
      ImportGroup
        { igName = Nothing,
          igRules = pure matchAllImportRule
        }

importGroupByQualifiedStrategy :: ImportGroups
importGroupByQualifiedStrategy =
  ImportGroups $
    NonEmpty.fromList
      [ ImportGroup
          { igName = Nothing,
            igRules = pure $ withUnqualifiedOnly matchAllImportRule
          },
        ImportGroup
          { igName = Nothing,
            igRules = pure $ withQualifiedOnly matchAllImportRule
          }
      ]

importGroupByScopeStrategy :: ImportGroups
importGroupByScopeStrategy =
  ImportGroups $
    NonEmpty.fromList
      [ ImportGroup
          { igName = Nothing,
            igRules = pure matchAllImportRule
          },
        ImportGroup
          { igName = Nothing,
            igRules = pure matchLocalModulesRule
          }
      ]

importGroupByScopeThenQualifiedStrategy :: ImportGroups
importGroupByScopeThenQualifiedStrategy =
  ImportGroups $
    NonEmpty.fromList
      [ ImportGroup
          { igName = Nothing,
            igRules = pure $ withQualified matchModule
          }
      | matchModule <- [matchAllImportRule, matchLocalModulesRule],
        withQualified <- [withUnqualifiedOnly, withQualifiedOnly]
      ]

groupsFromConfig :: Config.ImportGrouping -> ImportGroups
groupsFromConfig =
  \case
    Config.ImportGroupLegacy -> importGroupSingleStrategy
    Config.ImportGroupPreserve -> importGroupSingleStrategy
    Config.ImportGroupSingle -> importGroupSingleStrategy
    Config.ImportGroupByQualified -> importGroupByQualifiedStrategy
    Config.ImportGroupByScope -> importGroupByScopeStrategy
    Config.ImportGroupByScopeThenQualified -> importGroupByScopeThenQualifiedStrategy
    Config.ImportGroupCustom igs -> ImportGroups igs

matchAllImportRule :: ImportGroupRule
matchAllImportRule =
  ImportGroupRule
    { igrGlob = matchAllGlob,
      igrImportListMatcher = Config.MatchAnyImportDeclaration,
      igrQualifiedMatcher = Config.MatchBothQualifiedAndUnqualified,
      igrScopeMatcher = Config.MatchAllModules,
      igrPriority = Config.matchAllRulePriority
    }

matchLocalModulesRule :: ImportGroupRule
matchLocalModulesRule =
  ImportGroupRule
    { igrGlob = matchAllGlob,
      igrImportListMatcher = Config.MatchAnyImportDeclaration,
      igrQualifiedMatcher = Config.MatchBothQualifiedAndUnqualified,
      igrScopeMatcher = Config.MatchLocalModules,
      igrPriority = Config.matchLocalRulePriority
    }

withQualifiedOnly :: ImportGroupRule -> ImportGroupRule
withQualifiedOnly ImportGroupRule {..} =
  ImportGroupRule
    { igrQualifiedMatcher = Config.MatchQualifiedOnly,
      ..
    }

withUnqualifiedOnly :: ImportGroupRule -> ImportGroupRule
withUnqualifiedOnly ImportGroupRule {..} =
  ImportGroupRule
    { igrQualifiedMatcher = Config.MatchUnqualifiedOnly,
      ..
    }

matchesRule :: Set Cabal.ModuleName -> Import -> ImportGroupRule -> Bool
matchesRule localMods Import {..} ImportGroupRule {..} =
  and
    [ matchingGlob,
      matchingImportList,
      matchingQualified,
      matchingScope
    ]
  where
    matchingGlob = moduleNameString importName `matchesGlob` igrGlob
    matchingImportList = case igrImportListMatcher of
      Config.MatchExplicitImportList -> importList == Just ImportList
      Config.MatchHidingImportClause -> importList == Just HidingList
      Config.MatchWholeModuleImport -> importList == Nothing
      Config.MatchAnyImportDeclaration -> True
    matchingQualified = case igrQualifiedMatcher of
      Config.MatchQualifiedOnly -> importQualified
      Config.MatchUnqualifiedOnly -> not importQualified
      Config.MatchBothQualifiedAndUnqualified -> True
    matchingScope =
      let isLocalModule = ghcModuleNameToCabal importName `Set.member` localMods
       in case igrScopeMatcher of
            Config.MatchAllModules -> True
            Config.MatchExternalModules -> not isLocalModule
            Config.MatchLocalModules -> isLocalModule

data GroupImportsOpts = GroupImportsOpts
  { grouping :: ImportGrouping,
    respectful :: Bool,
    -- | All comments in the HsModule.
    --
    -- Can't retrieve comments from 'R', since 'R' runs the first time without
    -- comments.
    allComments :: [LEpaComment]
  }

prepareExistingGroups :: GroupImportsOpts -> [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
prepareExistingGroups opts =
  case opts.grouping of
    ImportGroupPreserve -> preserveGroups
    ImportGroupLegacy | opts.respectful -> preserveGroups
    _ -> flattenGroups
  where
    preserveGroups = map toList . groupBy' (\x y -> not $ separatedByBlank' x y)
    flattenGroups = pure

    -- separatedByBlank only checks if the span lines are more than 1 apart.
    -- If there's a comment between two imports with no blank lines, we should
    -- still consider it one import group.
    separatedByBlank' a b =
      fromMaybe False $ do
        endA <- srcSpanEndLine <$> srcSpanToRealSrcSpan (getLocA a)
        startB <- srcSpanStartLine <$> srcSpanToRealSrcSpan (getLocA b)
        pure . null . filter (not . hasComment) $ [endA + 1 .. startB - 1]

    -- Maps startLine -> endLine
    commentLineIntervals =
      Map.fromList
        [ (srcSpanStartLine spn, srcSpanEndLine spn)
        | comment <- opts.allComments,
          let spn = epaLocationRealSrcSpan $ getLoc comment
        ]
    hasComment lineNum =
      (not . Map.null)
        -- Find any comment where: startLine <= lineNum <= endLine
        . Map.filter (>= lineNum)
        . Map.takeWhileAntitone (<= lineNum)
        $ commentLineIntervals

groupImports :: forall x. GroupImportsOpts -> Set Cabal.ModuleName -> (x -> Import) -> [x] -> [[x]]
groupImports opts localModules fToImport = regroup . fmap (breakTies . matchRules)
  where
    ImportGroups igs = groupsFromConfig opts.grouping

    indexedGroupRules :: [(Int, [ImportGroupRule])]
    indexedGroupRules = zip [0 ..] (toList . igRules <$> toList igs)

    matchRules :: x -> ([(Int, [ImportGroupRule])], x)
    matchRules x =
      let imp = fToImport x
          testRule (_, rules) = any (matchesRule localModules imp) rules
       in (filter testRule indexedGroupRules, x)

    breakTies :: ([(Int, [ImportGroupRule])], x) -> (Int, x)
    breakTies ([], x) =
      (maxBound, x) -- Any non-matched imports will be in the last group
    breakTies (matches, x) =
      (fst . minimumBy (compare `on` snd) $ second (minimum . fmap igrPriority) <$> matches, x)

    regroup :: [(Int, x)] -> [[x]]
    regroup = fmap (fmap snd) . groupBy ((==) `on` fst) . sortOn fst
