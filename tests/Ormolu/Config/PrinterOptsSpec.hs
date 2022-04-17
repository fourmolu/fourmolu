{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tests for Fourmolu configuration options. Similar to PrinterSpec.hs
--
-- Writing as a separate file to avoid merge conflicts in PrinterSpec.hs. This
-- way, Fourmolu can implement its tests independently of how Ormolu does its
-- testing.
module Ormolu.Config.PrinterOptsSpec (spec) where

import Control.Monad (forM_, when)
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu
  ( Config (..),
    PrinterOpts (..),
    PrinterOptsTotal,
    defaultConfig,
    defaultPrinterOpts,
    detectSourceType,
    ormolu,
  )
import Ormolu.Utils.IO (readFileUtf8, writeFileUtf8)
import Path
  ( File,
    Path,
    fromRelFile,
    parseRelDir,
    parseRelFile,
    toFilePath,
    (</>),
  )
import Path.IO (doesFileExist)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import qualified Text.PrettyPrint as Doc

data TestGroup = forall a.
  TestGroup
  { label :: String,
    testCases :: [a],
    updateConfig :: a -> PrinterOptsTotal -> PrinterOptsTotal,
    showTestCase :: a -> String,
    testCaseSuffix :: a -> String
  }

spec :: Spec
spec =
  mapM_
    runTestGroup
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
          testCaseSuffix = suffix2
        },
      TestGroup
        { label = "comma-style",
          testCases = allOptions,
          updateConfig = \commaStyle opts -> opts {poCommaStyle = pure commaStyle},
          showTestCase = show,
          testCaseSuffix = suffix1
        },
      TestGroup
        { label = "import-export",
          testCases = (,) <$> allOptions <*> allOptions,
          updateConfig = \(commaStyle, diffFriendly) opts ->
            opts
              { poImportExportCommaStyle = pure commaStyle,
                poDiffFriendlyImportExport = pure diffFriendly
              },
          showTestCase = \(commaStyle, diffFriendly) ->
            show commaStyle ++ if diffFriendly then " + diff friendly" else "",
          testCaseSuffix = suffix2
        },
      TestGroup
        { label = "record-brace-space",
          testCases = allOptions,
          updateConfig = \recordBraceSpace opts -> opts {poRecordBraceSpace = pure recordBraceSpace},
          showTestCase = show,
          testCaseSuffix = suffix1
        },
      TestGroup
        { label = "respectful",
          testCases = allOptions,
          updateConfig = \respectful opts -> opts {poRespectful = pure respectful},
          showTestCase = show,
          testCaseSuffix = suffix1
        },
      TestGroup
        { label = "haddock-style",
          testCases = allOptions,
          updateConfig = \haddockStyle opts -> opts {poHaddockStyle = pure haddockStyle},
          showTestCase = show,
          testCaseSuffix = suffix1
        }
    ]
  where
    allOptions :: (Enum a, Bounded a) => [a]
    allOptions = [minBound .. maxBound]

    suffixWith xs = concatMap ('-' :) xs
    suffix1 a1 = suffixWith [show a1]
    suffix2 (a1, a2) = suffixWith [show a1, show a2]

runTestGroup :: TestGroup -> Spec
runTestGroup TestGroup {..} =
  describe label $
    forM_ testCases $ \testCase ->
      it ("generates the correct output for: " ++ showTestCase testCase) $ do
        let inputFile = testDir </> toRelFile "input.hs"
            inputPath = fromRelFile inputFile
            outputFile = testDir </> toRelFile ("output" ++ testCaseSuffix testCase ++ ".hs")
            outputPath = fromRelFile outputFile
            config =
              defaultConfig
                { cfgPrinterOpts = updateConfig testCase defaultPrinterOpts,
                  cfgSourceType = detectSourceType inputPath,
                  cfgCheckIdempotence = True
                }
            runOrmolu path = ormolu config path . T.unpack

        input <- readFileUtf8 inputPath
        actual <- runOrmolu inputPath input
        mExpected <- getFileContents outputFile
        case (shouldRegenerateOutput, mExpected) of
          (False, Nothing) ->
            expectationFailure "Output does not exist. Try running with ORMOLU_REGENERATE_EXAMPLES=1"
          (False, Just expected) ->
            when (actual /= expected) $
              expectationFailure . T.unpack $
                getDiff ("actual", actual) ("expected", expected)
          (True, _) ->
            writeFileUtf8 outputPath actual
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

{--- Helpers ---}

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

shouldRegenerateOutput :: Bool
shouldRegenerateOutput =
  -- Use same env var as PrinterSpec.hs, to make it easy to regenerate everything at once
  unsafePerformIO $ isJust <$> lookupEnv "ORMOLU_REGENERATE_EXAMPLES"
{-# NOINLINE shouldRegenerateOutput #-}
