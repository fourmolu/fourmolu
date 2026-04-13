{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Fourmolu file configuration.
module Ormolu.ConfigSpec (spec) where

import Control.Monad ((>=>))
import Data.ByteString.Char8 qualified as Char8
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Ormolu.Config
import Ormolu.Fixity (ModuleReexports (..))
import Ormolu.Utils.Glob (matchAllGlob, mkGlob)
import Test.Hspec

spec :: Spec
spec = do
  describe "FourmoluConfig" $ do
    it "parses multiple reexports from same module" $ do
      config <-
        Yaml.decodeThrow . Char8.pack . unlines $
          [ "reexports:",
            "- module Foo exports Bar1",
            "- module Foo exports Bar2"
          ]
      let expected =
            Map.fromList
              [ ("Foo", NonEmpty.fromList [(Nothing, "Bar2"), (Nothing, "Bar1")])
              ]
      cfgFileReexports config `shouldBe` ModuleReexports expected
    it "applies configurations in correct order" $ do
      let opts1 = mempty {poIndentation = Just 2}
          opts2 = mempty {poIndentation = Just 4}
          configs = [opts1, opts2]
      poIndentation (resolvePrinterOpts configs) `shouldBe` 4

    context "when using an import grouping configuration" $ do
      it "parses 'legacy' as the 'ImportGroupLegacy' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: legacy"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just ImportGroupLegacy
      it "parses 'preserve' as the 'ImportGroupPreserve' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: preserve"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just ImportGroupPreserve
      it "parses 'single' as the 'ImportGroupSingle' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: single"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just ImportGroupSingle
      it "parses 'by-qualified' as the 'ImportGroupByQualified' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: by-qualified"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just ImportGroupByQualified
      it "parses 'by-scope' as the 'ImportGroupByScope' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: by-scope"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just ImportGroupByScope
      it "parses 'by-scope-then-qualified' as the 'ImportGroupByScopeThenQualified' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: by-scope-then-qualified"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just ImportGroupByScopeThenQualified
      it "fails when an unknown strategy is requested" $ do
        let decodeResult = Yaml.decodeEither' @FourmoluConfig "import-grouping: fake-strategy"
            isAnUnknownStrategyValue e = case e of
              Left (Yaml.AesonException msg) -> "unknown strategy value" `isInfixOf` msg
              _ -> False
        decodeResult `shouldSatisfy` isAnUnknownStrategyValue
      it "parses a grouping rule's name" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - name: Some name",
              "    rules:",
              "      - {}"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Just "Some name",
                      igRules = NonEmpty.fromList [defaultImportGroupRule]
                    }
                ]
        actualStrategy `shouldBe` Just (ImportGroupCustom expectedRules)

      let checkRule :: (HasCallStack) => String -> String -> ImportGroupRule -> SpecWith ()
          checkRule desc yamlRuleConfig expectedRuleConfig =
            it ("parses " ++ desc) $ do
              let yamlConfig =
                    unlines
                      [ "import-grouping:",
                        "  - rules:",
                        "      - " ++ yamlRuleConfig
                      ]

                  downCustomRules (ImportGroupCustom customRules) = Just customRules
                  downCustomRules _ = Nothing
                  downSingleton (a :| []) = Just a
                  downSingleton _ = Nothing
                  accessTestedRule = downCustomRules >=> downSingleton >=> downSingleton . igRules

              config <- Yaml.decodeThrow (Char8.pack yamlConfig)

              let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
              case actualStrategy >>= accessTestedRule of
                Nothing -> expectationFailure "A single tested rule change was expected"
                Just actualRule -> actualRule `shouldBe` expectedRuleConfig

          checkRuleAttribute :: (HasCallStack) => String -> String -> (ImportGroupRule -> ImportGroupRule) -> SpecWith ()
          checkRuleAttribute desc yamlRuleConfig applyExpectedChange = do
            let baseArbitraryYamlConfig = "glob: \"**\""
                yamlConfig = "{ " ++ baseArbitraryYamlConfig ++ ", " ++ yamlRuleConfig ++ "}"

                baseArbitraryConfig = defaultImportGroupRule {igrGlob = matchAllGlob}
                expectedRuleConfig = applyExpectedChange baseArbitraryConfig
            checkRule
              ("rules with the " ++ desc ++ " rule option")
              yamlConfig
              expectedRuleConfig

      checkRule "the 'glob' rule" "glob: Data.Text.**" defaultImportGroupRule {igrGlob = mkGlob "Data.Text.**"}
      checkRule "the default 'glob' rule (match all modules)" "{}" defaultImportGroupRule {igrGlob = mkGlob "**"}

      checkRuleAttribute "'any' import list" "import-list: any" $ \config -> config {igrImportListMatcher = MatchAnyImportDeclaration}
      checkRuleAttribute "'explicit' import list" "import-list: explicit" $ \config -> config {igrImportListMatcher = MatchExplicitImportList}
      checkRuleAttribute "'hiding' import list" "import-list: hiding" $ \config -> config {igrImportListMatcher = MatchHidingImportClause}
      checkRuleAttribute "'none' import list" "import-list: none" $ \config -> config {igrImportListMatcher = MatchWholeModuleImport}
      checkRuleAttribute "default import list" "" $ \config -> config {igrImportListMatcher = MatchAnyImportDeclaration}

      checkRuleAttribute "'qualified: yes'" "qualified: yes" $ \config -> config {igrQualifiedMatcher = MatchQualifiedOnly}
      checkRuleAttribute "'qualified: no'" "qualified: no" $ \config -> config {igrQualifiedMatcher = MatchUnqualifiedOnly}
      checkRuleAttribute "default 'qualified' (match any)" "" $ \config -> config {igrQualifiedMatcher = MatchBothQualifiedAndUnqualified}

      checkRuleAttribute "'scope: any'" "scope: any" $ \config -> config {igrScopeMatcher = MatchAllModules}
      checkRuleAttribute "'scope: local'" "scope: local" $ \config -> config {igrScopeMatcher = MatchLocalModules}
      checkRuleAttribute "'scope: external'" "scope: external" $ \config -> config {igrScopeMatcher = MatchExternalModules}
      checkRuleAttribute "default 'scope' (match all)" "" $ \config -> config {igrScopeMatcher = MatchAllModules}

      checkRuleAttribute "'priority' (55 priority example)" "priority: 55" $ \config -> config {igrPriority = ImportRulePriority 55}
      checkRuleAttribute "'priority' (80 priority example)" "priority: 80" $ \config -> config {igrPriority = ImportRulePriority 80}
      checkRuleAttribute "default 'priority' (50)" "" $ \config -> config {igrPriority = ImportRulePriority 50}

      checkRule
        "the legacy 'match' rule for all modules (with a 100 priority)"
        "match: all"
        defaultImportGroupRule {igrGlob = mkGlob "**", igrScopeMatcher = MatchAllModules, igrPriority = ImportRulePriority 100}
      checkRule
        "the legacy 'match' rule for local modules"
        "match: local-modules"
        defaultImportGroupRule {igrGlob = mkGlob "**", igrScopeMatcher = MatchLocalModules, igrPriority = ImportRulePriority 60}

      it "parses multiple group configurations and rules in their order of appearance in the configuration" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - name: Text modules",
              "    rules:",
              "      - glob: Data.Text",
              "  - name: The rest",
              "    rules:",
              "      - match: all",
              "  - name: My internals and monads unqualified",
              "    rules:",
              "      - match: local-modules",
              "        qualified: no",
              "      - glob: Control.Monad",
              "        qualified: no",
              "  - name: My internals and monads qualified",
              "    rules:",
              "      - match: local-modules",
              "        qualified: yes",
              "      - glob: Control.Monad",
              "        qualified: yes",
              "  - name: Specific monads",
              "    rules:",
              "      - glob: Control.Monad.**"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Just "Text modules",
                      igRules = NonEmpty.fromList [defaultImportGroupRule {igrGlob = mkGlob "Data.Text"}]
                    },
                  ImportGroup
                    { igName = Just "The rest",
                      igRules = NonEmpty.fromList [defaultImportGroupRule {igrGlob = matchAllGlob, igrPriority = ImportRulePriority 100}]
                    },
                  ImportGroup
                    { igName = Just "My internals and monads unqualified",
                      igRules =
                        NonEmpty.fromList
                          [ defaultImportGroupRule {igrQualifiedMatcher = MatchUnqualifiedOnly, igrScopeMatcher = MatchLocalModules, igrPriority = ImportRulePriority 60},
                            defaultImportGroupRule {igrGlob = mkGlob "Control.Monad", igrQualifiedMatcher = MatchUnqualifiedOnly}
                          ]
                    },
                  ImportGroup
                    { igName = Just "My internals and monads qualified",
                      igRules =
                        NonEmpty.fromList
                          [ defaultImportGroupRule {igrQualifiedMatcher = MatchQualifiedOnly, igrScopeMatcher = MatchLocalModules, igrPriority = ImportRulePriority 60},
                            defaultImportGroupRule {igrGlob = mkGlob "Control.Monad", igrQualifiedMatcher = MatchQualifiedOnly}
                          ]
                    },
                  ImportGroup
                    { igName = Just "Specific monads",
                      igRules = NonEmpty.fromList [defaultImportGroupRule {igrGlob = mkGlob "Control.Monad.**"}]
                    }
                ]
        actualStrategy `shouldBe` Just (ImportGroupCustom expectedRules)
      it "fails when a rule cannot be identified" $ do
        let decodeResult =
              Yaml.decodeEither' @FourmoluConfig . Char8.pack . unlines $
                [ "import-grouping:",
                  "  - rules:",
                  "      - match: whatever"
                ]
            isAnUnknownModuleMatcher e = case e of
              Left (Yaml.AesonException msg) -> "Unknown legacy match value" `isInfixOf` msg
              _ -> False
        decodeResult `shouldSatisfy` isAnUnknownModuleMatcher

defaultImportGroupRule :: ImportGroupRule
defaultImportGroupRule =
  ImportGroupRule
    { igrGlob = matchAllGlob,
      igrImportListMatcher = MatchAnyImportDeclaration,
      igrQualifiedMatcher = MatchBothQualifiedAndUnqualified,
      igrScopeMatcher = MatchAllModules,
      igrPriority = defaultImportRulePriority
    }
