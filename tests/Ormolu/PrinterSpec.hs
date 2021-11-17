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

spec :: Spec
spec = do
  defaultExamples <- runIO locateExamples
  let ormoluOpts =
        PrinterOpts
          { poIndentation = pure 2,
            poCommaStyle = pure Trailing,
            poIndentWheres = pure True,
            poRecordBraceSpace = pure True,
            poDiffFriendlyImportExport = pure False,
            poRespectful = pure False,
            poHaddockStyle = pure HaddockSingleLine,
            poNewlinesBetweenDecls = pure 1,
            poColumnLimit = pure NoLimit
          }

      lineBreakingExamples = map addSuffixForLineBreaking defaultExamples
      withLineBreaking = defaultPrinterOpts {poColumnLimit = pure $ ColumnLimit 80}
  sequence_ $
    ( uncurry
        checkExample
        <$> [(ormoluOpts, ""), (defaultPrinterOpts, "-four")]
        <*> defaultExamples
    )
      <> (uncurry (checkExample withLineBreaking) <$> lineBreakingExamples)

addSuffixForLineBreaking :: Path Rel File -> (String, Path Rel File)
addSuffixForLineBreaking path =
  if any (`isSuffixOf` toFilePath path) exceptionList
    then ("-line-break", path)
    else ("-four", path)
  where
    exceptionList =
      [ "rewrite-rule/prelude1.hs",
        "rewrite-rule/prelude4.hs",
        "rewrite-rule/type-signature.hs",
        "column-limit-tests.hs"
      ]

-- | Check a single given example.
checkExample :: (PrinterOptsTotal, String, String) -> Path Rel File -> Spec
checkExample (po, label, suffix) srcPath' = it (fromRelFile srcPath' ++ " works (" ++ label ++ ")") . withNiceExceptions $ do
  let srcPath = examplesDir </> srcPath'
      cfg = defaultConfig {cfgPrinterOpts = po}
  expectedOutputPath <- deriveOutput suffix srcPath
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
  -- 4. Check that running the formatter on the output produces the same
  -- output again (the transformation is idempotent).
  formatted1 <- ormolu cfg "<formatted>" (T.unpack formatted0)
  -- Skip the column limit option test case, because it has a known limitation
  -- of breaking idempotence.
  unless ("column-limit-tests.hs" `isSuffixOf` toFilePath srcPath) $
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
deriveOutput suffix path =
  parseRelFile $
    F.addExtension (F.dropExtensions (fromRelFile path) ++ suffix ++ "-out") "hs"

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
