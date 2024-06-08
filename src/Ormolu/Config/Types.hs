{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Config.Types
  ( ImportGroup (..),
    ImportGroupRule (..),
    ImportModuleMatcher (..),
    ImportRulePriority (..),
    defaultImportRulePriority,
    QualifiedImportMatcher (..),
  )
where

import Control.Applicative (Alternative (..), asum)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Ormolu.Utils.Glob (Glob, mkGlob)

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
    igrQualified :: !QualifiedImportMatcher,
    igrPriority :: !ImportRulePriority
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ImportGroupRule where
  parseJSON = Aeson.withObject "rule" $ \o -> do
    let parseModuleMatcher = Aeson.parseJSON (Aeson.Object o)
        failUnknownModuleMatcher = Aeson.parseFail "Unknown or invalid module matcher"
        attemptParseModuleMatcher = parseModuleMatcher <|> failUnknownModuleMatcher
    igrModuleMatcher <- attemptParseModuleMatcher

    qualified <- o .:? "qualified"
    igrQualified <- case qualified of
      Just True -> pure MatchQualifiedOnly
      Just False -> pure MatchUnqualifiedOnly
      Nothing -> pure MatchBothQualifiedAndUnqualified

    igrPriority <- o .:? "priority" .!= defaultImportRulePriority

    pure ImportGroupRule {..}

newtype ImportRulePriority = ImportRulePriority Word8
  deriving (Eq, Ord, Show, Bounded)

instance Aeson.FromJSON ImportRulePriority where
  parseJSON = fmap ImportRulePriority . Aeson.parseJSON

defaultImportRulePriority :: ImportRulePriority
defaultImportRulePriority = ImportRulePriority 50

data QualifiedImportMatcher
  = MatchQualifiedOnly
  | MatchUnqualifiedOnly
  | MatchBothQualifiedAndUnqualified
  deriving (Eq, Show)

data ImportModuleMatcher
  = MatchAllModules
  | MatchLocalModules
  | MatchGlob !Glob
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
        rawGlob <- o .: "glob"
        let glob = mkGlob rawGlob
        pure (MatchGlob glob)
