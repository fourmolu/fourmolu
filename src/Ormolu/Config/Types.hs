{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Config.Types
  ( ImportGroup (..),
    ImportGroupRule (..),
    ImportRulePriority (..),
    matchAllRulePriority,
    matchLocalRulePriority,
    defaultImportRulePriority,
    ImportListMatcher (..),
    QualifiedImportMatcher (..),
    ImportScopeMatcher (..),
  )
where

import Control.Applicative (Alternative (..))
import Data.Aeson ((.!=), (.:?))
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
  { igrGlob :: !Glob,
    igrImportListMatcher :: !ImportListMatcher,
    igrQualifiedMatcher :: !QualifiedImportMatcher,
    igrScopeMatcher :: !ImportScopeMatcher,
    igrPriority :: !ImportRulePriority
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ImportGroupRule where
  parseJSON = Aeson.withObject "rule" $ \o -> do
    legacyMatch <- o .:? "match"
    (defaultPriority, defaultScope) <- case legacyMatch of
      Just "all" -> pure (matchAllRulePriority, pure MatchAllModules)
      Just "local-modules" -> pure (matchLocalRulePriority, pure MatchLocalModules)
      Just other -> Aeson.parseFail $ "Unknown legacy match value: " <> other
      Nothing -> pure (defaultImportRulePriority, empty)

    igrGlob <- mkGlob <$> o .:? "glob" .!= "**"

    igrImportListMatcher <-
      o .:? "import-list" .!= "any" >>= \case
        "any" -> pure MatchAnyImportDeclaration
        "explicit" -> pure MatchExplicitImportList
        "hiding" -> pure MatchHidingImportClause
        "none" -> pure MatchWholeModuleImport
        other -> Aeson.parseFail $ "Unknown import list matcher: " <> other

    igrQualifiedMatcher <-
      o .:? "qualified" >>= \case
        Just True -> pure MatchQualifiedOnly
        Just False -> pure MatchUnqualifiedOnly
        Nothing -> pure MatchBothQualifiedAndUnqualified

    igrScopeMatcher <-
      o .:? "scope" >>= \case
        Just "any" -> pure MatchAllModules
        Just "local" -> pure MatchLocalModules
        Just "external" -> pure MatchExternalModules
        Nothing -> defaultScope <|> pure MatchAllModules
        Just other -> Aeson.parseFail ("Unknown scope matcher: " <> other)

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

data ImportListMatcher
  = MatchExplicitImportList
  | MatchHidingImportClause
  | MatchWholeModuleImport
  | MatchAnyImportDeclaration
  deriving (Eq, Show)

data QualifiedImportMatcher
  = MatchQualifiedOnly
  | MatchUnqualifiedOnly
  | MatchBothQualifiedAndUnqualified
  deriving (Eq, Show)

data ImportScopeMatcher
  = MatchExternalModules
  | MatchLocalModules
  | MatchAllModules
  deriving (Eq, Show)
