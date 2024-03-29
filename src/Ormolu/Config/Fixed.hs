module Ormolu.Config.Fixed
  ( ImportGroup (..),
    ImportGroupPreset (..),
    ImportGroupRule (..),
    ImportModuleMatcher (..),
    ImportRulePriority (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)

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
    igrQualified :: !(Maybe Bool),
    igrPriority :: !(Maybe ImportRulePriority)
  }
  deriving (Eq, Show)

data ImportModuleMatcher
  = MatchAllModules
  | MatchDefinedModules
  | RegexModuleMatcher !String
  deriving (Eq, Show)

newtype ImportRulePriority = ImportRulePriority Word8
  deriving (Eq, Show)
