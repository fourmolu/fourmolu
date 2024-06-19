{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Config.Types
  ( ImportGroup (..),
    ImportGroupRule (..),
    ImportModuleMatcher (..),
    ImportRulePriority (..),
    matchAllRulePriority,
    matchLocalRulePriority,
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
    igrQualifiedMatcher :: !QualifiedImportMatcher,
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
    igrQualifiedMatcher <- case qualified of
      Just True -> pure MatchQualifiedOnly
      Just False -> pure MatchUnqualifiedOnly
      Nothing -> pure MatchBothQualifiedAndUnqualified

    let defaultPriority
          | MatchAllModules <- igrModuleMatcher = matchAllRulePriority
          | otherwise = defaultImportRulePriority
    igrPriority <- o .:? "priority" .!= defaultPriority

    pure ImportGroupRule {..}

newtype ImportRulePriority = ImportRulePriority Word8
  deriving (Eq, Ord, Show, Bounded)

instance Aeson.FromJSON ImportRulePriority where
  parseJSON = fmap ImportRulePriority . Aeson.parseJSON

matchAllRulePriority :: ImportRulePriority
matchAllRulePriority = ImportRulePriority 100

matchLocalRulePriority :: ImportRulePriority
matchLocalRulePriority = ImportRulePriority 60 -- Lower priority than "all" but higher than the default.

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
