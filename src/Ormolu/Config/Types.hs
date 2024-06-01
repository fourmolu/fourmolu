-- | This module contains configuration data types that are not meant to be
-- provided directly as CLI arguments or through the configuration GUI but as
-- more advanced YAML options.
module Ormolu.Config.Types
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
  = -- | Preset that will group every import declarations not matched by custom rules
    AllPreset
  deriving (Eq, Show)

data ImportGroupRule = ImportGroupRule
  { igrModuleMatcher :: !ImportModuleMatcher,
    -- | 'Just True' to match qualified declarations, 'Just False' to match unqualified ones and 'Nothing' to match both
    igrQualified :: !(Maybe Bool)
  }
  deriving (Eq, Show)

data ImportModuleMatcher
  = MatchAllModules
  | MatchLocalModules
  | MatchGlob !String
  deriving (Eq, Show)
