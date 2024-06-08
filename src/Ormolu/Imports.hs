{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Manipulations on import lists.
module Ormolu.Imports
  ( ImportGroups,
    importGroupSingleStrategy,
    groupsFromConfig,
    normalizeImports,
  )
where

import Data.Bifunctor
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, minimumBy, nubBy, sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.ModuleName qualified as Cabal
import GHC.Data.FastString
import GHC.Hs
import GHC.Hs.ImpExp as GHC
import GHC.Types.Name.Reader
import GHC.Types.PkgQual
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Config qualified as Config
import Ormolu.Utils (ghcModuleNameToCabal, groupBy', notImplemented, separatedByBlank, showOutputable)
import Ormolu.Utils.Glob (Glob, matchesGlob)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

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

groupsFromConfig :: Set Cabal.ModuleName -> Config.ImportGrouping -> ImportGroups
groupsFromConfig localModules =
  \case
    Config.ImportGroupSingle -> importGroupSingleStrategy
    Config.ImportGroupByQualified -> importGroupByQualifiedStrategy
    Config.ImportGroupByScope -> importGroupByScopeStrategy localModules
    Config.ImportGroupByScopeThenQualified -> importGroupByScopeThenQualifiedStrategy localModules
    Config.ImportGroupCustom igs -> ImportGroups $ convertImportGroup <$> igs
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

matchesRule :: ImportId -> ImportGroupRule -> Bool
matchesRule ImportId {..} ImportGroupRule {..} = matchesModules && matchesQualified
  where
    matchesModules = case igrModuleMatcher of
      MatchAllModules -> True
      MatchModules mods -> ghcModuleNameToCabal importIdName `Set.member` mods
      MatchGlobModule gl -> moduleNameString importIdName `matchesGlob` gl
    matchesQualified = case igrQualifiedMatcher of
      Config.MatchQualifiedOnly -> importQualified
      Config.MatchUnqualifiedOnly -> not importQualified
      Config.MatchBothQualifiedAndUnqualified -> True

-- | Sort, group and normalize imports.
--
-- Assumes input list is sorted by source location. Output list is not necessarily
-- sorted by source location, so this function should be called at most once on a
-- given input list.
normalizeImports :: Bool -> ImportGroups -> [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
normalizeImports preserveGroups importGroups =
  map (fmap snd)
    . concatMap
      ( groupImports importGroups
          . M.toAscList
          . M.fromListWith combineImports
          . fmap (\x -> (importId x, g x))
      )
    . if preserveGroups
      then map toList . groupBy' (\x y -> not $ separatedByBlank getLocA x y)
      else pure
  where
    g :: LImportDecl GhcPs -> LImportDecl GhcPs
    g (L l ImportDecl {..}) =
      L
        l
        ImportDecl
          { ideclImportList = second (fmap normalizeLies) <$> ideclImportList,
            ..
          }

groupImports :: ImportGroups -> [(ImportId, x)] -> [[(ImportId, x)]]
groupImports (ImportGroups igs) = regroup . fmap (breakTies . matchRules)
  where
    indexedGroupRules :: [(Int, [ImportGroupRule])]
    indexedGroupRules = zip [0 ..] (toList . igRules <$> toList igs)

    matchRules :: (ImportId, x) -> ([(Int, [ImportGroupRule])], (ImportId, x))
    matchRules (iid, x) =
      let testRule (_, rules) = any (matchesRule iid) rules
       in (filter testRule indexedGroupRules, (iid, x))

    breakTies :: ([(Int, [ImportGroupRule])], x) -> (Int, x)
    breakTies ([], x) =
      (maxBound, x) -- Any non-matched imports will be in the last group
    breakTies (matches, x) =
      (fst . minimumBy (compare `on` snd) $ second (minimum . fmap igrPriority) <$> matches, x)

    regroup :: [(Int, x)] -> [[x]]
    regroup = fmap (fmap snd) . groupBy ((==) `on` fst) . sortOn fst

-- | Combine two import declarations. It should be assumed that 'ImportId's
-- are equal.
combineImports ::
  LImportDecl GhcPs ->
  LImportDecl GhcPs ->
  LImportDecl GhcPs
combineImports (L lx ImportDecl {..}) (L _ y) =
  L
    lx
    ImportDecl
      { ideclImportList = case (ideclImportList, GHC.ideclImportList y) of
          (Just (hiding, L l' xs), Just (_, L _ ys)) ->
            Just (hiding, (L l' (normalizeLies (xs ++ ys))))
          _ -> Nothing,
        ..
      }

-- | Import id, a collection of all things that justify having a separate
-- import entry. This is used for merging of imports. If two imports have
-- the same 'ImportId' they can be merged.
data ImportId = ImportId
  { importIsPrelude :: Bool,
    importPkgQual :: ImportPkgQual,
    importIdName :: ModuleName,
    importSource :: IsBootInterface,
    importSafe :: Bool,
    importQualified :: Bool,
    importAs :: Maybe ModuleName,
    importHiding :: Maybe ImportListInterpretationOrd
  }
  deriving (Eq, Ord)

data ImportPkgQual
  = -- | The import is not qualified by a package name.
    NoImportPkgQual
  | -- | The import is qualified by an external package name.
    ImportPkgQual LexicalFastString
  | -- | The import is qualified by the current package being built, using the
    -- special @this@ package name.
    ImportPkgQualThis
  deriving stock (Eq, Ord)

mkImportPkgQual :: RawPkgQual -> ImportPkgQual
mkImportPkgQual = \case
  NoRawPkgQual -> NoImportPkgQual
  RawPkgQual (sl_fs -> fs)
    | fs == mkFastString "this" -> ImportPkgQualThis
    | otherwise -> ImportPkgQual (LexicalFastString fs)

-- | 'ImportListInterpretation' does not have an 'Ord' instance.
newtype ImportListInterpretationOrd = ImportListInterpretationOrd
  { unImportListInterpretationOrd :: ImportListInterpretation
  }
  deriving stock (Eq)

instance Ord ImportListInterpretationOrd where
  compare = compare `on` toBool . unImportListInterpretationOrd
    where
      toBool Exactly = False
      toBool EverythingBut = True

-- | Obtain an 'ImportId' for a given import.
importId :: LImportDecl GhcPs -> ImportId
importId (L _ ImportDecl {..}) =
  ImportId
    { importIsPrelude = isPrelude,
      importIdName = moduleName,
      importPkgQual = mkImportPkgQual ideclPkgQual,
      importSource = ideclSource,
      importSafe = ideclSafe,
      importQualified = case ideclQualified of
        QualifiedPre -> True
        QualifiedPost -> True
        NotQualified -> False,
      importAs = unLoc <$> ideclAs,
      importHiding = ImportListInterpretationOrd . fst <$> ideclImportList
    }
  where
    isPrelude = moduleNameString moduleName == "Prelude"
    moduleName = unLoc ideclName

-- | Normalize a collection of import items.
normalizeLies :: [LIE GhcPs] -> [LIE GhcPs]
normalizeLies = sortOn (getIewn . unLoc) . M.elems . foldl' combine M.empty
  where
    combine ::
      Map IEWrappedNameOrd (LIE GhcPs) ->
      LIE GhcPs ->
      Map IEWrappedNameOrd (LIE GhcPs)
    combine m (L new_l new) =
      let wname = getIewn new
          normalizeWNames =
            nubBy (\x y -> compareLIewn x y == EQ) . sortBy compareLIewn
          alter = \case
            Nothing -> Just . L new_l $
              case new of
                IEThingWith x n wildcard g _ ->
                  IEThingWith x n wildcard (normalizeWNames g) Nothing
                other -> other
            Just old ->
              let f = \case
                    IEVar _ n _ -> IEVar Nothing n Nothing
                    IEThingAbs _ _ _ -> new
                    IEThingAll x n _ -> IEThingAll x n Nothing
                    IEThingWith _ n wildcard g _ ->
                      case new of
                        IEVar _ _ _ ->
                          error "Ormolu.Imports broken presupposition"
                        IEThingAbs x _ _ ->
                          IEThingWith x n wildcard g Nothing
                        IEThingAll x n' _ ->
                          IEThingAll x n' Nothing
                        IEThingWith x n' wildcard' g' _ ->
                          let combinedWildcard =
                                case (wildcard, wildcard') of
                                  (IEWildcard _, _) -> IEWildcard 0
                                  (_, IEWildcard _) -> IEWildcard 0
                                  _ -> NoIEWildcard
                           in IEThingWith
                                x
                                n'
                                combinedWildcard
                                (normalizeWNames (g <> g'))
                                Nothing
                        IEModuleContents _ _ -> notImplemented "IEModuleContents"
                        IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                        IEDoc NoExtField _ -> notImplemented "IEDoc"
                        IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
                    IEModuleContents _ _ -> notImplemented "IEModuleContents"
                    IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                    IEDoc NoExtField _ -> notImplemented "IEDoc"
                    IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
               in Just (f <$> old)
       in M.alter alter wname m

-- | A wrapper for @'IEWrappedName' 'GhcPs'@ that allows us to define an
-- 'Ord' instance for it.
newtype IEWrappedNameOrd = IEWrappedNameOrd (IEWrappedName GhcPs)
  deriving (Eq)

instance Ord IEWrappedNameOrd where
  compare (IEWrappedNameOrd x) (IEWrappedNameOrd y) = compareIewn x y

-- | Project @'IEWrappedName' 'GhcPs'@ from @'IE' 'GhcPs'@.
getIewn :: IE GhcPs -> IEWrappedNameOrd
getIewn = \case
  IEVar _ x _ -> IEWrappedNameOrd (unLoc x)
  IEThingAbs _ x _ -> IEWrappedNameOrd (unLoc x)
  IEThingAll _ x _ -> IEWrappedNameOrd (unLoc x)
  IEThingWith _ x _ _ _ -> IEWrappedNameOrd (unLoc x)
  IEModuleContents _ _ -> notImplemented "IEModuleContents"
  IEGroup NoExtField _ _ -> notImplemented "IEGroup"
  IEDoc NoExtField _ -> notImplemented "IEDoc"
  IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"

-- | Like 'compareIewn' for located wrapped names.
compareLIewn :: LIEWrappedName GhcPs -> LIEWrappedName GhcPs -> Ordering
compareLIewn = compareIewn `on` unLoc

-- | Compare two @'IEWrapppedName' 'GhcPs'@ things.
compareIewn :: IEWrappedName GhcPs -> IEWrappedName GhcPs -> Ordering
compareIewn (IEName _ x) (IEName _ y) = unLoc x `compareRdrName` unLoc y
compareIewn (IEName _ _) (IEPattern _ _) = LT
compareIewn (IEName _ _) (IEType _ _) = LT
compareIewn (IEPattern _ _) (IEName _ _) = GT
compareIewn (IEPattern _ x) (IEPattern _ y) = unLoc x `compareRdrName` unLoc y
compareIewn (IEPattern _ _) (IEType _ _) = LT
compareIewn (IEType _ _) (IEName _ _) = GT
compareIewn (IEType _ _) (IEPattern _ _) = GT
compareIewn (IEType _ x) (IEType _ y) = unLoc x `compareRdrName` unLoc y

compareRdrName :: RdrName -> RdrName -> Ordering
compareRdrName x y =
  case (getNameStr x, getNameStr y) of
    ([], []) -> EQ
    ((_ : _), []) -> GT
    ([], (_ : _)) -> LT
    ((x' : _), (y' : _)) ->
      case (isAlphaNum x', isAlphaNum y') of
        (False, False) -> x `compare` y
        (True, False) -> LT
        (False, True) -> GT
        (True, True) -> x `compare` y
  where
    getNameStr = showOutputable . rdrNameOcc
