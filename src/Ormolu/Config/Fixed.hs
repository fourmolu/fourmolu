module Ormolu.Config.Fixed
    ( ImportGroup (..),
      ImportGroupRule (..),
      ImportModuleMatcher (..),
      ImportTieBreaker (..),
    )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)

data ImportGroup = ImportGroup
    { igName :: !(Maybe String),
      igRules :: !(NonEmpty ImportGroupRule)
    }
    deriving (Eq, Show)

data ImportGroupRule = ImportGroupRule
    { igrModuleMatcher :: !ImportModuleMatcher,
      igrQualified :: !(Maybe Bool),
      igrTieBreaker :: !(Maybe ImportTieBreaker)
    }
    deriving (Eq, Show)

data ImportModuleMatcher
    = MatchAllModules
    | MatchDefinedModules
    | RegexModuleMatcher !String
    deriving (Eq, Show)

newtype ImportTieBreaker = ImportTieBreaker Word8
    deriving (Eq, Show)
