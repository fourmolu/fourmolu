{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tests for Fourmolu configuration options. Similar to PrinterSpec.hs
--
-- Writing as a separate file to avoid merge conflicts in PrinterSpec.hs. This
-- way, Fourmolu can implement its tests independently of how Ormolu does its
-- testing.
module Ormolu.Config.PrinterOptsSpec (spec) where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Data.Algorithm.DiffContext qualified as Diff
import Data.Char (isSpace)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T.Utf8
import Distribution.ModuleName qualified as ModuleName
import Distribution.Types.PackageName (mkPackageName)
import GHC.Stack (withFrozenCallStack)
import Ormolu (detectSourceType, ormolu)
import Ormolu.Config
import Ormolu.Exception (OrmoluException, printOrmoluException)
import Ormolu.Terminal (runTerm)
import Ormolu.Utils.Glob (mkGlob)
import Path
  ( File,
    Path,
    Rel,
    fromRelFile,
    parseRelDir,
    parseRelFile,
    toFilePath,
    (</>),
  )
import Path.IO (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Text.PrettyPrint qualified as Doc

data TestGroup
  = forall a.
  TestGroup
  { label :: String,
    -- | When True, takes input from 'input-multi.hs' instead of 'input.hs', where sections
    -- delimited by `{- // -}` will be formatted as separate Haskell files. Useful for testing
    -- combinations of module headers, which is normally only allowed once.
    isMulti :: Bool,
    testCases :: [a],
    updateConfig :: a -> PrinterOptsTotal -> PrinterOptsTotal,
    showTestCase :: a -> [String],
    checkIdempotence :: Bool
  }

-- Tests where each test group is a directory with an `input.hs` file and multiple `output-*.hs`
-- files that could be regenerated with ORMOLU_REGENERATE_EXAMPLES.
spec :: Spec
spec =
  mapM_ runTestGroup $
    [ TestGroup
        { label = "indentation",
          isMulti = False,
          testCases = (,) <$> [2, 3, 4] <*> allOptions,
          updateConfig = \(indent, indentWheres) opts ->
            opts
              { poIndentation = pure indent,
                poIndentWheres = pure indentWheres
              },
          showTestCase = \(indent, indentWheres) ->
            [ renderPrinterOpt indent,
              if indentWheres then "indent_wheres" else ""
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "column-limit",
          isMulti = True,
          testCases = [NoLimit, ColumnLimit 80, ColumnLimit 100],
          updateConfig = \columnLimit opts -> opts {poColumnLimit = pure columnLimit},
          showTestCase = \columnLimit ->
            [ "limit=" ++ renderColumnLimit columnLimit
            ],
          checkIdempotence = False
        },
      TestGroup
        { label = "function-arrows",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \functionArrows opts ->
            opts {poFunctionArrows = pure functionArrows},
          showTestCase = \functionArrows ->
            [ renderPrinterOpt functionArrows
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "comma-style",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \commaStyle opts -> opts {poCommaStyle = pure commaStyle},
          showTestCase = \commaStyle ->
            [ renderPrinterOpt commaStyle
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "import-export",
          isMulti = True,
          testCases = allOptions,
          updateConfig = \commaStyle opts ->
            opts {poImportExportStyle = pure commaStyle},
          showTestCase = \commaStyle ->
            [ renderPrinterOpt commaStyle
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "import-grouping",
          isMulti = False,
          testCases =
            [ ImportGroupPreserve,
              ImportGroupSingle,
              ImportGroupByQualified,
              ImportGroupByScope,
              ImportGroupByScopeThenQualified,
              ImportGroupCustom . NonEmpty.fromList $
                [ ImportGroup
                    { igName = Nothing,
                      igRules =
                        NonEmpty.fromList
                          [ ImportGroupRule
                              { igrModuleMatcher = MatchGlob (mkGlob "Data.Text"),
                                igrQualifiedMatcher = MatchBothQualifiedAndUnqualified,
                                igrPriority = defaultImportRulePriority
                              }
                          ]
                    },
                  ImportGroup
                    { igName = Nothing,
                      igRules =
                        NonEmpty.fromList
                          [ ImportGroupRule
                              { igrModuleMatcher = MatchAllModules,
                                igrQualifiedMatcher = MatchBothQualifiedAndUnqualified,
                                igrPriority = ImportRulePriority 100
                              }
                          ]
                    },
                  ImportGroup
                    { igName = Nothing,
                      igRules =
                        NonEmpty.fromList
                          [ ImportGroupRule
                              { igrModuleMatcher = MatchGlob (mkGlob "SomeInternal.**"),
                                igrQualifiedMatcher = MatchQualifiedOnly,
                                igrPriority = defaultImportRulePriority
                              },
                            ImportGroupRule
                              { igrModuleMatcher = MatchGlob (mkGlob "Unknown.**"),
                                igrQualifiedMatcher = MatchUnqualifiedOnly,
                                igrPriority = defaultImportRulePriority
                              }
                          ]
                    },
                  ImportGroup
                    { igName = Nothing,
                      igRules =
                        NonEmpty.fromList
                          [ ImportGroupRule
                              { igrModuleMatcher = MatchLocalModules,
                                igrQualifiedMatcher = MatchUnqualifiedOnly,
                                igrPriority = defaultImportRulePriority
                              },
                            ImportGroupRule
                              { igrModuleMatcher = MatchAllModules,
                                igrQualifiedMatcher = MatchQualifiedOnly,
                                igrPriority = defaultImportRulePriority
                              }
                          ]
                    }
                ]
            ],
          updateConfig = \igs opts ->
            opts
              { poImportGrouping = pure igs
              },
          showTestCase = \igs ->
            [ renderImportGrouping igs
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "record-brace-space",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \recordBraceSpace opts -> opts {poRecordBraceSpace = pure recordBraceSpace},
          showTestCase = \recordBraceSpace ->
            [ renderPrinterOpt recordBraceSpace
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "newlines-between-decls",
          isMulti = False,
          testCases = (,) <$> [0, 1, 2] <*> allOptions,
          updateConfig = \(newlines, respectful) opts ->
            opts
              { poNewlinesBetweenDecls = pure newlines,
                poRespectful = pure respectful
              },
          showTestCase = \(newlines, respectful) ->
            [ renderPrinterOpt newlines,
              if respectful then "respectful" else ""
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "haddock-style",
          isMulti = False,
          testCases = (,) <$> allOptions <*> (PrintStyleInherit : map PrintStyleOverride allOptions),
          updateConfig = \(haddockStyle, haddockStyleModule) opts ->
            opts
              { poHaddockStyle = pure haddockStyle,
                poHaddockStyleModule = pure haddockStyleModule
              },
          showTestCase = \(haddockStyle, haddockStyleModule) ->
            [ renderPrinterOpt haddockStyle,
              case haddockStyleModule of
                PrintStyleInherit -> ""
                PrintStyleOverride style -> "module=" ++ renderPrinterOpt style
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "haddock-location-signature",
          isMulti = False,
          testCases = (,,) <$> allOptions <*> allOptions <*> allOptions,
          updateConfig = \(functionArrows, haddockLocSig, isMulti) opts ->
            opts
              { poHaddockStyle = pure $ if isMulti then HaddockMultiLine else HaddockSingleLine,
                poFunctionArrows = pure functionArrows,
                poHaddockLocSignature = pure haddockLocSig
              },
          showTestCase = \(functionArrows, haddockLocSig, isMulti) ->
            [ "arrows=" ++ renderPrinterOpt functionArrows,
              case haddockLocSig of
                HaddockLocSigAuto -> ""
                _ -> "haddock=" ++ renderPrinterOpt haddockLocSig,
              if isMulti then "multi" else ""
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "let-style",
          isMulti = False,
          testCases = (,,) <$> allOptions <*> allOptions <*> [2, 4],
          updateConfig = \(letStyle, inStyle, indent) opts ->
            opts
              { poIndentation = pure indent,
                poLetStyle = pure letStyle,
                poInStyle = pure inStyle
              },
          showTestCase = \(letStyle, inStyle, indent) ->
            [ "let=" ++ renderPrinterOpt letStyle,
              "in=" ++ renderPrinterOpt inStyle,
              "indent=" ++ renderPrinterOpt indent
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "if-style",
          isMulti = False,
          testCases = (,) <$> allOptions <*> [2, 4],
          updateConfig = \(ifStyle, indent) opts ->
            opts
              { poIndentation = pure indent,
                poIfStyle = pure ifStyle
              },
          showTestCase = \(ifStyle, indent) ->
            [ renderPrinterOpt ifStyle,
              "indent=" ++ renderPrinterOpt indent
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "single-constraint-parens",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \parens opts -> opts {poSingleConstraintParens = pure parens},
          showTestCase = \parens ->
            [ renderPrinterOpt parens
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "single-deriving-parens",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \parens opts -> opts {poSingleDerivingParens = pure parens},
          showTestCase = \parens ->
            [ renderPrinterOpt parens
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "sort-constraints",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \sortConstraints options -> options {poSortConstraints = pure sortConstraints},
          showTestCase = \sortConstraints ->
            [ renderPrinterOpt sortConstraints
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "sort-derived-classes",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \sortDerivedClasses opts -> opts {poSortDerivedClasses = pure sortDerivedClasses},
          showTestCase = \sortDerivedClasses ->
            [ renderPrinterOpt sortDerivedClasses
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "sort-deriving-clauses",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \sortDerivingClauses opts -> opts {poSortDerivingClauses = pure sortDerivingClauses},
          showTestCase = \sortDerivingClauses ->
            [ renderPrinterOpt sortDerivingClauses
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "trailing-section-operators",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \trailingSectionOperators opts -> opts {poTrailingSectionOperators = pure trailingSectionOperators},
          showTestCase = \trailingSectionOperators ->
            [ renderPrinterOpt trailingSectionOperators
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "unicode-syntax",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \unicodePreference options -> options {poUnicode = pure unicodePreference},
          showTestCase = \unicodePreference ->
            [ renderPrinterOpt unicodePreference
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "respectful",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \respectful opts -> opts {poRespectful = pure respectful},
          showTestCase = \respectful ->
            [ renderPrinterOpt respectful
            ],
          checkIdempotence = True
        },
      TestGroup
        { label = "respectful-module-where",
          isMulti = True,
          testCases = (,) <$> allOptions <*> allOptions,
          updateConfig = \(respectful, importExportStyle) opts ->
            opts
              { poRespectful = pure respectful,
                poImportExportStyle = pure importExportStyle
              },
          showTestCase = \(respectful, importExportStyle) ->
            [ "respectful=" ++ renderPrinterOpt respectful,
              renderPrinterOpt importExportStyle
            ],
          checkIdempotence = True
        }
    ]

runTestGroup :: TestGroup -> Spec
runTestGroup TestGroup {..} =
  describe label $
    forM_ testCases $ \testCase ->
      it ("generates the correct output for: " ++ toTestLabel testCase) $ do
        let inputFile = testDir </> toRelFile (if isMulti then "input-multi.hs" else "input.hs")
            inputPath = fromRelFile inputFile
            outputFile = testDir </> toRelFile ("output-" ++ toTestFileSuffix testCase ++ ".hs")
            opts = updateConfig testCase defaultPrinterOpts

        input <- T.Utf8.readFile inputPath
        actual <-
          if isMulti
            then overSectionsM (T.pack "{- // -}") (runOrmolu opts checkIdempotence inputPath) input
            else runOrmolu opts checkIdempotence inputPath input
        checkResult outputFile actual
  where
    testDir = toRelDir $ "data/fourmolu/" ++ label
    toRelDir name =
      case parseRelDir name of
        Just path -> path
        Nothing -> error $ "Not a valid directory name: " ++ show name
    toRelFile name =
      case parseRelFile name of
        Just path -> path
        Nothing -> error $ "Not a valid file name: " ++ show name

    toTestLabel = renderTestCase (T.pack " + ") id
    toTestFileSuffix = renderTestCase (T.pack "-") (T.replace (T.pack "-") (T.pack "_"))
    renderTestCase delim f = T.unpack . T.intercalate delim . filter (not . T.null) . map (f . T.pack) . showTestCase

runOrmolu :: PrinterOptsTotal -> Bool -> FilePath -> Text -> IO Text
runOrmolu opts checkIdempotence inputPath input =
  ormolu config inputPath input `catch` \e -> do
    msg <- renderOrmoluException e
    expectationFailure' $ unlines ["Got ormolu exception:", "", msg]
  where
    config =
      defaultConfig
        { cfgPrinterOpts = opts,
          cfgSourceType = detectSourceType inputPath,
          cfgCheckIdempotence = checkIdempotence,
          cfgDependencies = S.fromList [mkPackageName "base"],
          cfgLocalModules =
            S.fromList $
              ModuleName.fromString
                <$> [ "SomeInternal.Module1",
                      "SomeInternal.Module1.SubModuleA",
                      "SomeInternal.Module2"
                    ]
        }

checkResult :: Path Rel File -> Text -> Expectation
checkResult outputFile actual
  | shouldRegenerateOutput = T.Utf8.writeFile (fromRelFile outputFile) actual
  | otherwise =
      getFileContents outputFile >>= \case
        Nothing ->
          expectationFailure "Output does not exist. Try running with ORMOLU_REGENERATE_EXAMPLES=1"
        Just expected ->
          when (actual /= expected) $
            expectationFailure . T.unpack $
              getDiff ("actual", actual) ("expected", expected)

{--- Renderers ---}

renderImportGrouping :: ImportGrouping -> String
renderImportGrouping igs = case igs of
  ImportGroupCustom _ -> "custom"
  ImportGroupLegacy -> "legacy"
  ImportGroupPreserve -> "preserve"
  ImportGroupSingle -> "single"
  ImportGroupByScope -> "by-scope"
  ImportGroupByQualified -> "by-qualified"
  ImportGroupByScopeThenQualified -> "by-scope-then-qualified"

renderColumnLimit :: ColumnLimit -> String
renderColumnLimit = \case
  NoLimit -> "none"
  ColumnLimit x -> show x

{--- Helpers ---}

allOptions :: (Enum a, Bounded a) => [a]
allOptions = [minBound .. maxBound]

overSectionsM :: (Monad m) => Text -> (Text -> m Text) -> Text -> m Text
overSectionsM delim f = go . T.lines
  where
    go inputLines =
      case break (== delim) inputLines of
        (section, []) -> f $ T.unlines section
        (pre, _ : post) -> do
          let (section, delimPre) = spanEnd (T.all isSpace) pre
              (delimPost, rest) = span (T.all isSpace) post

          resultPre <- f $ T.unlines section
          let delimLines = T.unlines $ concat [delimPre, [delim], delimPost]
          resultPost <- go rest
          pure $ resultPre <> delimLines <> resultPost

getFileContents :: Path b File -> IO (Maybe Text)
getFileContents path = do
  fileExists <- doesFileExist path
  if fileExists
    then Just <$> T.Utf8.readFile (toFilePath path)
    else pure Nothing

getDiff :: (String, Text) -> (String, Text) -> Text
getDiff (s1Name, s1) (s2Name, s2) =
  T.pack . Doc.render $
    Diff.prettyContextDiff (Doc.text s1Name) (Doc.text s2Name) (Doc.text . T.unpack . Diff.unnumber) $
      Diff.getContextDiff (Just 2) (T.lines s1) (T.lines s2)

renderOrmoluException :: OrmoluException -> IO String
renderOrmoluException e =
  withSystemTempFile "PrinterOptsSpec" $ \fp handle -> do
    runTerm (printOrmoluException e) Never handle
    hClose handle
    readFile fp

expectationFailure' :: (HasCallStack) => String -> IO a
expectationFailure' msg = do
  withFrozenCallStack $ expectationFailure msg
  -- satisfy type-checker, since hspec's expectationFailure is IO ()
  error "unreachable"

shouldRegenerateOutput :: Bool
shouldRegenerateOutput =
  -- Use same env var as PrinterSpec.hs, to make it easy to regenerate everything at once
  unsafePerformIO $ isJust <$> lookupEnv "ORMOLU_REGENERATE_EXAMPLES"
{-# NOINLINE shouldRegenerateOutput #-}

{--- Utilities ---}

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd f xs =
  let xs' = reverse xs
   in (reverse $ dropWhile f xs', reverse $ takeWhile f xs')
