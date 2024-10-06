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
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.Char (isSpace)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T.Utf8
import Distribution.ModuleName qualified as ModuleName
import GHC.Stack (withFrozenCallStack)
import Ormolu
  ( Config (..),
    PrinterOpts (..),
    PrinterOptsTotal,
    defaultConfig,
    defaultPrinterOpts,
    detectSourceType,
    ormolu,
  )
import Ormolu.Config (ColumnLimit (..), HaddockPrintStyleModule (..), ImportGroup (..), ImportGroupRule (..), ImportGrouping (..), ImportModuleMatcher (..), ImportRulePriority (ImportRulePriority), QualifiedImportMatcher (MatchBothQualifiedAndUnqualified, MatchQualifiedOnly, MatchUnqualifiedOnly), defaultImportRulePriority)
import Ormolu.Exception (OrmoluException, printOrmoluException)
import Ormolu.Terminal (ColorMode (..), runTerm)
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
import Text.Printf (printf)

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
    showTestCase :: a -> String,
    testCaseSuffix :: a -> String,
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
            show indent ++ if indentWheres then " + indent wheres" else "",
          testCaseSuffix = \(indent, indentWheres) ->
            suffixWith [show indent, if indentWheres then "indent_wheres" else ""],
          checkIdempotence = True
        },
      TestGroup
        { label = "column-limit",
          isMulti = True,
          testCases = [NoLimit, ColumnLimit 80, ColumnLimit 100],
          updateConfig = \columnLimit opts -> opts {poColumnLimit = pure columnLimit},
          showTestCase = show,
          testCaseSuffix = \columnLimit ->
            let limitStr =
                  case columnLimit of
                    NoLimit -> "none"
                    ColumnLimit x -> show x
             in suffixWith ["limit=" ++ limitStr],
          checkIdempotence = False
        },
      TestGroup
        { label = "function-arrows",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \functionArrows opts ->
            opts {poFunctionArrows = pure functionArrows},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "comma-style",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \commaStyle opts -> opts {poCommaStyle = pure commaStyle},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "import-export",
          isMulti = True,
          testCases = allOptions,
          updateConfig = \commaStyle opts ->
            opts {poImportExportStyle = pure commaStyle},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "record-brace-space",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \recordBraceSpace opts -> opts {poRecordBraceSpace = pure recordBraceSpace},
          showTestCase = show,
          testCaseSuffix = suffix1,
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
            show newlines ++ if respectful then " (respectful)" else "",
          testCaseSuffix = \(newlines, respectful) ->
            suffixWith [show newlines, if respectful then "respectful" else ""],
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
            show haddockStyle
              ++ case haddockStyleModule of
                PrintStyleInherit -> ""
                PrintStyleOverride style -> " + module=" ++ show style,
          testCaseSuffix = \(haddockStyle, haddockStyleModule) ->
            suffixWith
              [ show haddockStyle,
                case haddockStyleModule of
                  PrintStyleInherit -> ""
                  PrintStyleOverride style -> "module=" ++ show style
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
            printf "%s + %s (indent=%d)" (show letStyle) (show inStyle) indent,
          testCaseSuffix = \(letStyle, inStyle, indent) ->
            suffixWith [show letStyle, show inStyle, "indent=" ++ show indent],
          checkIdempotence = True
        },
      TestGroup
        { label = "single-constraint-parens",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \parens opts -> opts {poSingleConstraintParens = pure parens},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "single-deriving-parens",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \parens opts -> opts {poSingleDerivingParens = pure parens},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "unicode-syntax",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \unicodePreference options -> options {poUnicode = pure unicodePreference},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "respectful",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \respectful opts -> opts {poRespectful = pure respectful},
          showTestCase = show,
          testCaseSuffix = suffix1,
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
            (if respectful then "respectful" else "not respectful") ++ " + " ++ show importExportStyle,
          testCaseSuffix = \(respectful, importExportStyle) ->
            suffixWith ["respectful=" ++ show respectful, show importExportStyle],
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
          showTestCase = showStrategy,
          testCaseSuffix = \igs -> suffixWith [showStrategy igs],
          checkIdempotence = True
        },
      TestGroup
        { label = "sort-constraints",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \sortConstraints options -> options {poSortConstraints = pure sortConstraints},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "sort-derived-classes",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \sortDerivedClasses opts -> opts {poSortDerivedClasses = pure sortDerivedClasses},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "sort-deriving-clauses",
          isMulti = False,
          testCases = allOptions,
          updateConfig = \sortDerivingClauses opts -> opts {poSortDerivingClauses = pure sortDerivingClauses},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        }
    ]

runTestGroup :: TestGroup -> Spec
runTestGroup TestGroup {..} =
  describe label $
    forM_ testCases $ \testCase ->
      it ("generates the correct output for: " ++ showTestCase testCase) $ do
        let inputFile = testDir </> toRelFile (if isMulti then "input-multi.hs" else "input.hs")
            inputPath = fromRelFile inputFile
            outputFile = testDir </> toRelFile ("output" ++ testCaseSuffix testCase ++ ".hs")
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

{--- Helpers ---}

allOptions :: (Enum a, Bounded a) => [a]
allOptions = [minBound .. maxBound]

suffixWith :: [String] -> String
suffixWith xs = concatMap ('-' :) . filter (not . null) $ xs

suffix1 :: (Show a) => a -> String
suffix1 a1 = suffixWith [show a1]

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
    prettyContextDiff (Doc.text s1Name) (Doc.text s2Name) (Doc.text . T.unpack) $
      getContextDiff 2 (T.lines s1) (T.lines s2)

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

showStrategy :: ImportGrouping -> String
showStrategy igs = case igs of
  ImportGroupCustom _ -> "custom"
  _ -> show igs
