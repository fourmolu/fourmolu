{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Imports.Grouping
  ( ImportGroups,
    Import (..),
    importGroupSingleStrategy,
    groupsFromConfig,
    groupImports,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, minimumBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.ModuleName qualified as Cabal
import Language.Haskell.Syntax (ModuleName, moduleNameString)
import Ormolu.Config qualified as Config
import Ormolu.Utils (ghcModuleNameToCabal)
import Ormolu.Utils.Glob (Glob, matchesGlob)

data Import = Import
  { importName :: ModuleName,
    importQualified :: Bool
  }

newtype ImportGroups = ImportGroups (NonEmpty ImportGroup)

data ImportGroup = ImportGroup
  { igName :: !(Maybe String),
    igRules :: !(NonEmpty ImportGroupRule)
  }

data ImportGroupRule = ImportGroupRule
  { igrModuleMatcher :: !ModuleMatcher,
    igrQualifiedMatcher :: !Config.QualifiedImportMatcher,
    igrPriority :: !Config.ImportRulePriority
  }

data ModuleMatcher
  = MatchAllModules
  | MatchModules !(Set Cabal.ModuleName)
  | MatchGlobModule !Glob

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

importGroupByScopeStrategy :: Set Cabal.ModuleName -> ImportGroups
importGroupByScopeStrategy mods =
  ImportGroups $
    NonEmpty.fromList
      [ ImportGroup
          { igName = Nothing,
            igRules = pure matchAllImportRule
          },
        ImportGroup
          { igName = Nothing,
            igRules = pure $ matchModulesRule mods
          }
      ]

importGroupByScopeThenQualifiedStrategy :: Set Cabal.ModuleName -> ImportGroups
importGroupByScopeThenQualifiedStrategy mods =
  ImportGroups $
    NonEmpty.fromList
      [ ImportGroup
          { igName = Nothing,
            igRules =
              pure $ withQualified matchModule
          }
        | matchModule <- [matchAllImportRule, matchModulesRule mods],
          withQualified <- [withUnqualifiedOnly, withQualifiedOnly]
      ]

groupsFromConfig :: Bool -> Set Cabal.ModuleName -> Config.ImportGrouping -> Maybe ImportGroups
groupsFromConfig respectful localModules =
  \case
    Config.ImportGroupLegacy -> bool (Just importGroupSingleStrategy) Nothing respectful
    Config.ImportGroupPreserve -> Nothing
    Config.ImportGroupSingle -> Just importGroupSingleStrategy
    Config.ImportGroupByQualified -> Just importGroupByQualifiedStrategy
    Config.ImportGroupByScope -> Just $ importGroupByScopeStrategy localModules
    Config.ImportGroupByScopeThenQualified -> Just $ importGroupByScopeThenQualifiedStrategy localModules
    Config.ImportGroupCustom igs -> Just . ImportGroups $ convertImportGroup <$> igs
  where
    convertImportGroup :: Config.ImportGroup -> ImportGroup
    convertImportGroup Config.ImportGroup {..} =
      ImportGroup
        { igName = igName,
          igRules = convertGroupRule <$> igRules
        }

    convertGroupRule :: Config.ImportGroupRule -> ImportGroupRule
    convertGroupRule Config.ImportGroupRule {..} =
      ImportGroupRule
        { igrModuleMatcher =
            case igrModuleMatcher of
              Config.MatchAllModules -> MatchAllModules
              Config.MatchLocalModules -> MatchModules localModules
              Config.MatchGlob gl -> MatchGlobModule gl,
          igrQualifiedMatcher = igrQualified,
          igrPriority = igrPriority
        }

matchAllImportRule :: ImportGroupRule
matchAllImportRule =
  ImportGroupRule
    { igrModuleMatcher = MatchAllModules,
      igrQualifiedMatcher = Config.MatchBothQualifiedAndUnqualified,
      igrPriority = Config.ImportRulePriority 100
    }

matchModulesRule :: Set Cabal.ModuleName -> ImportGroupRule
matchModulesRule mods =
  ImportGroupRule
    { igrModuleMatcher = MatchModules mods,
      igrQualifiedMatcher = Config.MatchBothQualifiedAndUnqualified,
      igrPriority = Config.ImportRulePriority 60 -- Lower priority than "all" but higher than the default.
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

matchesRule :: Import -> ImportGroupRule -> Bool
matchesRule Import {..} ImportGroupRule {..} = matchesModules && matchesQualified
  where
    matchesModules = case igrModuleMatcher of
      MatchAllModules -> True
      MatchModules mods -> ghcModuleNameToCabal importName `Set.member` mods
      MatchGlobModule gl -> moduleNameString importName `matchesGlob` gl
    matchesQualified = case igrQualifiedMatcher of
      Config.MatchQualifiedOnly -> importQualified
      Config.MatchUnqualifiedOnly -> not importQualified
      Config.MatchBothQualifiedAndUnqualified -> True

groupImports :: forall x. ImportGroups -> (x -> Import) -> [x] -> [[x]]
groupImports (ImportGroups igs) fToImport = regroup . fmap (breakTies . matchRules)
  where
    indexedGroupRules :: [(Int, [ImportGroupRule])]
    indexedGroupRules = zip [0 ..] (toList . igRules <$> toList igs)

    matchRules :: x -> ([(Int, [ImportGroupRule])], x)
    matchRules x =
      let imp = fToImport x
          testRule (_, rules) = any (matchesRule imp) rules
       in (filter testRule indexedGroupRules, x)

    breakTies :: ([(Int, [ImportGroupRule])], x) -> (Int, x)
    breakTies ([], x) =
      (maxBound, x) -- Any non-matched imports will be in the last group
    breakTies (matches, x) =
      (fst . minimumBy (compare `on` snd) $ second (minimum . fmap igrPriority) <$> matches, x)

    regroup :: [(Int, x)] -> [[x]]
    regroup = fmap (fmap snd) . groupBy ((==) `on` fst) . sortOn fst
