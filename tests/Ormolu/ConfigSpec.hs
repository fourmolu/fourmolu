{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Fourmolu file configuration.
module Ormolu.ConfigSpec (spec) where

import Data.ByteString.Char8 qualified as Char8
import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Ormolu.Config (FourmoluConfig (..), ImportGroup (..), ImportGroupPreset (..), ImportGroupRule (..), ImportGrouping (..), ImportModuleMatcher (..), PrinterOpts (..), resolvePrinterOpts)
import Ormolu.Fixity (ModuleReexports (..))
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
      it "parses 'single' as the 'CreateSingleGroup' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: single"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just CreateSingleGroup
      it "parses 'by-qualified' as the 'SplitByQualified' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: by-qualified"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just SplitByQualified
      it "parses 'by-scope' as the 'SplitByScope' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: by-scope"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just SplitByScope
      it "parses 'by-scope-then-qualified' as the 'SplitByScopeAndQualified' import grouping strategy" $ do
        config <- Yaml.decodeThrow "import-grouping: by-scope-then-qualified"
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
        actualStrategy `shouldBe` Just SplitByScopeAndQualified
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
              "    preset: all"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Just "Some name",
                      igPresetOrRules = Left AllPreset
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "parses the 'all' preset" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - preset: all"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Nothing,
                      igPresetOrRules = Left AllPreset
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "enables the 'qualified' rule option" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - rules:",
              "      - glob: \"**\"",
              "        qualified: yes"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Nothing,
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchGlob "**",
                                  igrQualified = Just True
                                }
                            ]
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "disables the 'qualified' rule option" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - rules:",
              "      - glob: \"**\"",
              "        qualified: no"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Nothing,
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchGlob "**",
                                  igrQualified = Just False
                                }
                            ]
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "parses a 'glob' rule" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - rules:",
              "      - glob: Data.Text.**"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Nothing,
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchGlob "Data.Text.**",
                                  igrQualified = Nothing
                                }
                            ]
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "parses a 'match' rule for local modules" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - rules:",
              "      - match: local-modules"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Nothing,
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchLocalModules,
                                  igrQualified = Nothing
                                }
                            ]
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "parses a 'match' rule for all modules" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - rules:",
              "      - match: all"
            ]
        let actualStrategy = poImportGrouping (cfgFilePrinterOpts config)
            expectedRules =
              NonEmpty.fromList
                [ ImportGroup
                    { igName = Nothing,
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchAllModules,
                                  igrQualified = Nothing
                                }
                            ]
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "parses multiple group configurations and rules in their order of appearance in the configuration" $ do
        config <-
          Yaml.decodeThrow . Char8.pack . unlines $
            [ "import-grouping:",
              "  - name: Text modules",
              "    rules:",
              "      - glob: Data.Text",
              "  - name: The rest",
              "    preset: all",
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
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchGlob "Data.Text",
                                  igrQualified = Nothing
                                }
                            ]
                    },
                  ImportGroup
                    { igName = Just "The rest",
                      igPresetOrRules = Left AllPreset
                    },
                  ImportGroup
                    { igName = Just "My internals and monads unqualified",
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchLocalModules,
                                  igrQualified = Just False
                                },
                              ImportGroupRule
                                { igrModuleMatcher = MatchGlob "Control.Monad",
                                  igrQualified = Just False
                                }
                            ]
                    },
                  ImportGroup
                    { igName = Just "My internals and monads qualified",
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchLocalModules,
                                  igrQualified = Just True
                                },
                              ImportGroupRule
                                { igrModuleMatcher = MatchGlob "Control.Monad",
                                  igrQualified = Just True
                                }
                            ]
                    },
                  ImportGroup
                    { igName = Just "Specific monads",
                      igPresetOrRules =
                        Right $
                          NonEmpty.fromList
                            [ ImportGroupRule
                                { igrModuleMatcher = MatchGlob "Control.Monad.**",
                                  igrQualified = Nothing
                                }
                            ]
                    }
                ]
        actualStrategy `shouldBe` Just (UseCustomImportGroups expectedRules)
      it "fails when a rule cannot be identified" $ do
        let decodeResult =
              Yaml.decodeEither' @FourmoluConfig . Char8.pack . unlines $
                [ "import-grouping:",
                  "  - rules:",
                  "      - some-unknown-rule-type: whatever"
                ]
            isAnUnknownModuleMatcher e = case e of
              Left (Yaml.AesonException msg) -> "Unknown or invalid module matcher" `isInfixOf` msg
              _ -> False
        decodeResult `shouldSatisfy` isAnUnknownModuleMatcher