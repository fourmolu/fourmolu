{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.PrinterSpec (spec) where

import Control.Exception
import Control.Monad
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu
import Ormolu.Config
import Ormolu.Utils.IO
import Path
import Path.IO
import qualified System.FilePath as F
import Test.Hspec

-- | A single test case: an input file and the suffix of the expected output file.
data FormattingTestCase = FormattingTestCase
  { -- | Input file path relative to `examplesDir`.
    inputFp :: Path Rel File,
    -- | Suffix of expected output file.
    outputFileSuffix :: String
  }

-- | A test set for a specific set of options, e.g. ormolu's default options,
-- fourmolu's default options.
data PrinterOptsTestSet = PrinterOptsTestSet
  { -- | Config/printer options being used, e.g. indentation, comma-style.
    po :: PrinterOptsTotal,
    -- | Test set label, e.g. "ormolu", "fourmolu", "feature X".
    label :: String,
    -- | Some customizations might break idepotence. In these cases, the
    -- idempotence check should be skipped.
    canBreakIdempotence :: Bool,
    -- | All input and expected output file pairs, i.e. the test cases.
    testFiles :: [FormattingTestCase]
  }

spec :: Spec
spec = do
  defaultExamples <- runIO locateExamples

  let allTestSets =
        sequence
          -- Add new test sets to this list.
          [ mkOrmoluOptsSet,
            mkDefaultOptsSet
          ]
          defaultExamples

  mapM_ runConfigTestSet allTestSets
  where
    runConfigTestSet :: PrinterOptsTestSet -> Spec
    runConfigTestSet PrinterOptsTestSet {..} =
      mapM_
        ( \FormattingTestCase {..} ->
            checkExample po label canBreakIdempotence outputFileSuffix inputFp
        )
        testFiles

-- | Check a single given example.
checkExample :: PrinterOptsTotal -> String -> Bool -> String -> Path Rel File -> Spec
checkExample po label disableIdempotenceCheck outputFileSuffix srcPath' = it (fromRelFile srcPath' ++ " works (" ++ label ++ ")") . withNiceExceptions $ do
  let srcPath = examplesDir </> srcPath'
      cfg = defaultConfig {cfgPrinterOpts = po}
  expectedOutputPath <- deriveOutput outputFileSuffix srcPath
  -- 1. Given input snippet of source code parse it and pretty print it.
  -- 2. Parse the result of pretty-printing again and make sure that AST
  -- is the same as AST of the original snippet. (This happens in
  -- 'ormoluFile' automatically.)
  formatted0 <- ormoluFile cfg (fromRelFile srcPath)
  -- 3. Check the output against expected output. Thus all tests should
  -- include two files: input and expected output.
  -- <<< UNCOMMENT NEXT LINE TO REGENERATE OUTPUT FILES >>>
  -- writeFile (fromRelFile expectedOutputPath) (T.unpack formatted0)
  expected <- readFileUtf8 $ fromRelFile expectedOutputPath
  shouldMatch False formatted0 expected

  unless disableIdempotenceCheck $ do
    -- 4. Check that running the formatter on the output produces the same
    -- output again (the transformation is idempotent).
    formatted1 <- ormolu cfg "<formatted>" (T.unpack formatted0)
    shouldMatch True formatted1 formatted0

-- | Build list of examples for testing.
locateExamples :: IO [Path Rel File]
locateExamples =
  filter isInput . snd <$> listDirRecurRel examplesDir

-- | Does given path look like input path (as opposed to expected output
-- path)?
isInput :: Path Rel File -> Bool
isInput path =
  let s = fromRelFile path
      (s', exts) = F.splitExtensions s
   in exts == ".hs" && not ("-out" `isSuffixOf` s')

-- | For given path of input file return expected name of output.
deriveOutput :: String -> Path Rel File -> IO (Path Rel File)
deriveOutput outputFileSuffix path =
  parseRelFile $
    F.addExtension (F.dropExtensions (fromRelFile path) ++ outputFileSuffix ++ "-out") "hs"

-- | A version of 'shouldBe' that is specialized to comparing 'Text' values.
-- It also prints multi-line snippets in a more readable form.
shouldMatch :: Bool -> Text -> Text -> Expectation
shouldMatch idempotenceTest actual expected =
  when (actual /= expected) . expectationFailure $
    unlines
      [ ">>>>>>>>>>>>>>>>>>>>>> expected (" ++ pass ++ "):",
        T.unpack expected,
        ">>>>>>>>>>>>>>>>>>>>>> but got:",
        T.unpack actual
      ]
  where
    pass =
      if idempotenceTest
        then "idempotence pass"
        else "first pass"

examplesDir :: Path Rel Dir
examplesDir = $(mkRelDir "data/examples")

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.
withNiceExceptions ::
  -- | Action that may throw the exception
  Expectation ->
  Expectation
withNiceExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO ()
    h = expectationFailure . displayException

-- --------------------------- Test sets ---------------------------

-- Test set config for ormolu's default options.
mkOrmoluOptsSet :: [Path Rel File] -> PrinterOptsTestSet
mkOrmoluOptsSet examples =
  PrinterOptsTestSet
    { po =
        PrinterOpts
          { poIndentation = pure 2,
            poCommaStyle = pure Trailing,
            poIndentWheres = pure True,
            poRecordBraceSpace = pure True,
            poDiffFriendlyImportExport = pure False,
            poRespectful = pure False,
            poHaddockStyle = pure HaddockSingleLine,
            poNewlinesBetweenDecls = pure 1
          },
      label = "ormolu",
      canBreakIdempotence = False,
      testFiles = map (`FormattingTestCase` "") examples
    }

-- Test set config for fourmolu's default options.
mkDefaultOptsSet :: [Path Rel File] -> PrinterOptsTestSet
mkDefaultOptsSet examples =
  PrinterOptsTestSet
    { po = defaultPrinterOpts,
      label = "fourmolu",
      canBreakIdempotence = False,
      testFiles = map (`FormattingTestCase` "-four") examples
    }
