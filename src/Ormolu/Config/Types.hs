{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Config.Types
  ( ImportGroup (..),
    ImportGroupRule (..),
    ImportModuleMatcher (..),
    ImportRulePriority (..),
  )
where

import Control.Applicative (Alternative (..), asum)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)

data ImportGroup = ImportGroup
  { igName :: !(Maybe String),
    igRules :: !(NonEmpty ImportGroupRule)
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ImportGroup where
  parseJSON = Aeson.withObject "ImportGroup" $ \o ->
    ImportGroup
      <$> Aeson.parseFieldMaybe o "name"
      <*> Aeson.parseField o "rules"

data ImportGroupRule = ImportGroupRule
  { igrModuleMatcher :: !ImportModuleMatcher,
    -- | 'Just True' to match qualified declarations, 'Just False' to match unqualified ones and 'Nothing' to match both
    igrQualified :: !(Maybe Bool),
    igrPriority :: !(Maybe ImportRulePriority)
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ImportGroupRule where
  parseJSON = Aeson.withObject "rule" $ \o ->
    let parseModuleMatcher = Aeson.parseJSON (Aeson.Object o)
        failUnknownModuleMatcher = Aeson.parseFail "Unknown or invalid module matcher"
        attemptParseModuleMatcher = parseModuleMatcher <|> failUnknownModuleMatcher
     in ImportGroupRule
          <$> attemptParseModuleMatcher
          <*> Aeson.parseFieldMaybe o "qualified"
          <*> Aeson.parseFieldMaybe o "priority"

newtype ImportRulePriority = ImportRulePriority Word8
  deriving (Eq, Ord, Show, Bounded)

instance Aeson.FromJSON ImportRulePriority where
  parseJSON = fmap ImportRulePriority . Aeson.parseJSON

data ImportModuleMatcher
  = MatchAllModules
  | MatchLocalModules
  | MatchGlob !String
  deriving (Eq, Show)

instance Aeson.FromJSON ImportModuleMatcher where
  parseJSON v =
    asum
      [ parseMatchModuleMatcher v,
        parseGlobModuleMatcher v
      ]
    where
      parseMatchModuleMatcher :: Aeson.Value -> Aeson.Parser ImportModuleMatcher
      parseMatchModuleMatcher = Aeson.withObject "ImportModuleMatcher" $ \o -> do
        c <- Aeson.parseField @String o "match"
        case c of
          "all" -> pure MatchAllModules
          "local-modules" -> pure MatchLocalModules
          other -> Aeson.parseFail $ "Unknown matcher: " <> other

      parseGlobModuleMatcher :: Aeson.Value -> Aeson.Parser ImportModuleMatcher
      parseGlobModuleMatcher = Aeson.withObject "ImportModuleMatcher" $ \o -> do
        MatchGlob
          <$> Aeson.parseField @String o "glob"
