{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Applicative (Alternative (..), asum)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text

data ImportGroup = ImportGroup
  { igName :: !(Maybe String),
    igPresetOrRules :: !(Either ImportGroupPreset (NonEmpty ImportGroupRule))
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ImportGroup where
  parseJSON = Aeson.withObject "ImportGroup" $ \o ->
    let parsePresetField = Aeson.parseField o "preset"
        parseRulesField = Aeson.parseField o "rules"
        parsePresetOrRules = (Left <$> parsePresetField) <|> (Right <$> parseRulesField)
     in ImportGroup
          <$> Aeson.parseFieldMaybe o "name"
          <*> parsePresetOrRules

data ImportGroupPreset
  = -- | Preset that will group every import declarations not matched by custom rules
    AllPreset
  deriving (Eq, Show)

instance Aeson.FromJSON ImportGroupPreset where
  parseJSON = Aeson.withText "ImportGroupPreset" $ \case
    "all" -> pure AllPreset
    other -> Aeson.parseFail $ "Unknown preset: " <> Text.unpack other

data ImportGroupRule = ImportGroupRule
  { igrModuleMatcher :: !ImportModuleMatcher,
    -- | 'Just True' to match qualified declarations, 'Just False' to match unqualified ones and 'Nothing' to match both
    igrQualified :: !(Maybe Bool)
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

data ImportModuleMatcher
  = MatchAllModules
  | MatchLocalModules
  | MatchGlob !String
  deriving (Eq, Show)

instance Aeson.FromJSON ImportModuleMatcher where
  parseJSON v =
    asum
      [ parseCabalModuleMatcher v,
        parseMatchModuleMatcher v,
        parseGlobModuleMatcher v
      ]
    where
      parseCabalModuleMatcher :: Aeson.Value -> Aeson.Parser ImportModuleMatcher
      parseCabalModuleMatcher = Aeson.withObject "ImportModuleMatcher" $ \o -> do
        c <- Aeson.parseField @String o "cabal"
        case c of
          "local-modules" -> pure MatchLocalModules
          other -> Aeson.parseFail $ "Unknown Cabal matching: " <> other

      parseMatchModuleMatcher :: Aeson.Value -> Aeson.Parser ImportModuleMatcher
      parseMatchModuleMatcher = Aeson.withObject "ImportModuleMatcher" $ \o -> do
        c <- Aeson.parseField @String o "match"
        case c of
          "all" -> pure MatchAllModules
          other -> Aeson.parseFail $ "Unknown matcher: " <> other

      parseGlobModuleMatcher :: Aeson.Value -> Aeson.Parser ImportModuleMatcher
      parseGlobModuleMatcher = Aeson.withObject "ImportModuleMatcher" $ \o -> do
        MatchGlob
          <$> Aeson.parseField @String o "glob"
