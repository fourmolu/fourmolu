{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Imports.Grouping
  ( Import (..),
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
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.ModuleName qualified as Cabal
import GHC.Hs (GhcPs, getLocA)
import Language.Haskell.Syntax (LImportDecl, ModuleName, moduleNameString)
import Ormolu.Config (ImportGroup (..), ImportGroupRule (..), ImportGrouping (..), ImportModuleMatcher (..))
import Ormolu.Config qualified as Config
import Ormolu.Utils (ghcModuleNameToCabal, groupBy', separatedByBlank)
import Ormolu.Utils.Glob (matchesGlob)

newtype ImportGroups = ImportGroups (NonEmpty ImportGroup)

data Import = Import
  { importName :: ModuleName,
    importQualified :: Bool
  }

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
    { igrModuleMatcher = MatchAllModules,
      igrQualifiedMatcher = Config.MatchBothQualifiedAndUnqualified,
      igrPriority = Config.matchAllRulePriority
    }

matchLocalModulesRule :: ImportGroupRule
matchLocalModulesRule =
  ImportGroupRule
    { igrModuleMatcher = MatchLocalModules,
      igrQualifiedMatcher = Config.MatchBothQualifiedAndUnqualified,
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
matchesRule localMods Import {..} ImportGroupRule {..} = matchesModules && matchesQualified
  where
    matchesModules = case igrModuleMatcher of
      MatchAllModules -> True
      MatchLocalModules -> ghcModuleNameToCabal importName `Set.member` localMods
      MatchGlob gl -> moduleNameString importName `matchesGlob` gl
    matchesQualified = case igrQualifiedMatcher of
      Config.MatchQualifiedOnly -> importQualified
      Config.MatchUnqualifiedOnly -> not importQualified
      Config.MatchBothQualifiedAndUnqualified -> True

prepareExistingGroups :: ImportGrouping -> Bool -> [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
prepareExistingGroups ig respectful =
  case ig of
    ImportGroupPreserve -> preserveGroups
    ImportGroupLegacy | respectful -> preserveGroups
    _ -> flattenGroups
  where
    preserveGroups = map toList . groupBy' (\x y -> not $ separatedByBlank getLocA x y)
    flattenGroups = pure

groupImports :: forall x. ImportGrouping -> Set Cabal.ModuleName -> (x -> Import) -> [x] -> [[x]]
groupImports ig localModules fToImport = regroup . fmap (breakTies . matchRules)
  where
    ImportGroups igs = groupsFromConfig ig

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
