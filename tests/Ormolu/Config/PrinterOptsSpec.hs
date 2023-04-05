{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
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
import Ormolu.Config (ColumnLimit (..), HaddockPrintStyleModule (..))
import Ormolu.Exception (OrmoluException, printOrmoluException)
import Ormolu.Terminal (ColorMode (..), runTerm)
import Ormolu.Utils.IO (readFileUtf8, writeFileUtf8)
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

data TestGroup = forall a.
  TestGroup
  { label :: String,
    testCases :: [a],
    updateConfig :: a -> PrinterOptsTotal -> PrinterOptsTotal,
    showTestCase :: a -> String,
    testCaseSuffix :: a -> String,
    checkIdempotence :: Bool
  }

spec :: Spec
spec =
  sequence_
    [ singleTests,
      multiTests
    ]

-- Tests where each test group is a directory with an `input.hs` file and multiple `output-*.hs`
-- files that could be regenerated with ORMOLU_REGENERATE_EXAMPLES.
singleTests :: Spec
singleTests =
  mapM_
    (runTestGroup False)
    [ TestGroup
        { label = "indentation",
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
          testCases = allOptions,
          updateConfig = \functionArrows opts ->
            opts {poFunctionArrows = pure functionArrows},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "comma-style",
          testCases = allOptions,
          updateConfig = \commaStyle opts -> opts {poCommaStyle = pure commaStyle},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "import-export",
          testCases = allOptions,
          updateConfig = \commaStyle opts ->
            opts {poImportExportStyle = pure commaStyle},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "record-brace-space",
          testCases = allOptions,
          updateConfig = \recordBraceSpace opts -> opts {poRecordBraceSpace = pure recordBraceSpace},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "newlines-between-decls",
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
          testCases = allOptions,
          updateConfig = \parens opts -> opts {poSingleConstraintParens = pure parens},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "unicode-syntax",
          testCases = allOptions,
          updateConfig = \unicodePreference options -> options {poUnicode = pure unicodePreference},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        },
      TestGroup
        { label = "respectful",
          testCases = allOptions,
          updateConfig = \respectful opts -> opts {poRespectful = pure respectful},
          showTestCase = show,
          testCaseSuffix = suffix1,
          checkIdempotence = True
        }
    ]

-- Same as 'singleTests', except with input taken from 'input-multi.hs', where sections
-- delimited by `{- // -}` will be formatted as separate Haskell files. Useful for testing
-- combinations of module headers, which is normally only allowed once.
multiTests :: Spec
multiTests =
  mapM_
    (runTestGroup True)
    [ TestGroup
        { label = "respectful-module-where",
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
        }
    ]

runTestGroup :: Bool -> TestGroup -> Spec
runTestGroup isMulti TestGroup {..} =
  describe label $
    forM_ testCases $ \testCase ->
      it ("generates the correct output for: " ++ showTestCase testCase) $ do
        let inputFile = testDir </> toRelFile (if isMulti then "input-multi.hs" else "input.hs")
            inputPath = fromRelFile inputFile
            outputFile = testDir </> toRelFile ("output" ++ testCaseSuffix testCase ++ ".hs")
            opts = updateConfig testCase defaultPrinterOpts

        input <- readFileUtf8 inputPath
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
          cfgCheckIdempotence = checkIdempotence
        }

checkResult :: Path Rel File -> Text -> Expectation
checkResult outputFile actual
  | shouldRegenerateOutput = writeFileUtf8 (fromRelFile outputFile) actual
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
overSectionsM delim f =
  fmap T.concat
    . mapM (\(s, isDelim) -> if isDelim then pure s else f s)
    . splitOnDelim delim

getFileContents :: Path b File -> IO (Maybe Text)
getFileContents path = do
  fileExists <- doesFileExist path
  if fileExists
    then Just <$> readFileUtf8 (toFilePath path)
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

-- | Group delimiter (including surrounding whitespace) and non-delimiter lines
-- and annotate lines with a Bool indicating if the group is a delimiter group
-- or not.
splitOnDelim :: Text -> Text -> [(Text, Bool)]
splitOnDelim delim =
  map (\(lineGroup, delimType) -> (T.unlines lineGroup, isDelim delimType))
    . collapseSpaces NonDelim
    . collapseSpaces Delim
    . groupWith toLineType
    . T.lines
  where
    toLineType line
      | T.all isSpace line = Space
      | line == delim = Delim
      | otherwise = NonDelim

    collapseSpaces delimType = \case
      (xs, Space) : (ys, ysType) : rest | ysType == delimType -> collapseSpaces delimType $ (xs ++ ys, delimType) : rest
      (xs, xsType) : (ys, Space) : rest | xsType == delimType -> collapseSpaces delimType $ (xs ++ ys, delimType) : rest
      x : rest -> x : collapseSpaces delimType rest
      [] -> []

    isDelim = \case
      Delim -> True
      NonDelim -> False
      Space -> error "isDelim called on Space, but all Spaces should've been eliminated at this point"

    -- Like 'NE.groupWith', except annotates group with comparator
    groupWith :: (Eq b) => (a -> b) -> [a] -> [([a], b)]
    groupWith f =
      let liftComparator xs = (map fst $ NE.toList xs, snd $ NE.head xs)
       in map liftComparator . NE.groupWith snd . map (\a -> (a, f a))

data LineType = Space | Delim | NonDelim
  deriving (Eq)
