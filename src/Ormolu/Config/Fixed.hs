module Ormolu.Config.Fixed
  ( ImportGroup (..),
    ImportGroupPreset (..),
    ImportGroupRule (..),
    ImportModuleMatcher (..),
  )
where

import Data.List.NonEmpty (NonEmpty)

data ImportGroup = ImportGroup
  { igName :: !(Maybe String),
    igPresetOrRules :: !(Either ImportGroupPreset (NonEmpty ImportGroupRule))
  }
  deriving (Eq, Show)

data ImportGroupPreset
  = AllPreset
  deriving (Eq, Show)

data ImportGroupRule = ImportGroupRule
  { igrModuleMatcher :: !ImportModuleMatcher,
    igrQualified :: !(Maybe Bool)
  }
  deriving (Eq, Show)

data ImportModuleMatcher
  = MatchAllModules
  | MatchDefinedModules
  | MatchGlob !String
  deriving (Eq, Show)
