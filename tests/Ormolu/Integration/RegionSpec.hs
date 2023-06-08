{-# LANGUAGE RecordWildCards #-}

module Ormolu.Integration.RegionSpec (spec) where

import Control.Monad (forM_)
import Ormolu.Integration.Utils (getFourmoluExe, readProcess)
import System.Directory (copyFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | Tests for the `region-tests/` directory Ormolu implemented, converted
-- from nix tests to hspec tests.
spec :: Spec
spec =
  describe "region-tests" . beforeAll getFourmoluExe $
    forM_ tests $ \Test {..} ->
      specify testLabel $ \fourmoluExe -> do
        withSystemTempDirectory "region-test-dir" $ \tmpdir -> do
          let configFile = tmpdir ++ "/fourmolu.yaml"
              srcFile = tmpdir ++ "/input.hs"

          copyFile "region-tests/src.hs" srcFile
          copyFile "fourmolu.yaml" configFile

          actual <-
            readProcess fourmoluExe $
              [srcFile, "--check-idempotence"] ++ testArgs
          expected <- readFile $ "region-tests/" ++ testExpectedFileName
          actual `shouldBe` expected

data Test = Test
  { testLabel :: String,
    testArgs :: [String],
    testExpectedFileName :: FilePath
  }

tests :: [Test]
tests =
  [ Test
      { testLabel = "Works with implicit arguments",
        testArgs = [],
        testExpectedFileName = "expected-result-all.hs"
      },
    Test
      { testLabel = "Works with explicit arguments",
        testArgs = ["--start-line", "1", "--end-line", "23"],
        testExpectedFileName = "expected-result-all.hs"
      },
    Test
      { testLabel = "Works with only --start-line",
        testArgs = ["--start-line", "1"],
        testExpectedFileName = "expected-result-all.hs"
      },
    Test
      { testLabel = "Works with only --end-line",
        testArgs = ["--end-line", "23"],
        testExpectedFileName = "expected-result-all.hs"
      },
    Test
      { testLabel = "Works with lines 8-9",
        testArgs = ["--start-line", "8", "--end-line", "9"],
        testExpectedFileName = "expected-result-8-9.hs"
      },
    Test
      { testLabel = "Works with lines 8-10",
        testArgs = ["--start-line", "8", "--end-line", "10"],
        testExpectedFileName = "expected-result-8-10.hs"
      },
    Test
      { testLabel = "Works with lines 11-14",
        testArgs = ["--start-line", "11", "--end-line", "14"],
        testExpectedFileName = "expected-result-11-14.hs"
      },
    Test
      { testLabel = "Works with lines 19-23",
        testArgs = ["--start-line", "19", "--end-line", "23"],
        testExpectedFileName = "expected-result-19-23.hs"
      }
  ]
