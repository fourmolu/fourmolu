{- FOURMOLU_DISABLE -} -- https://github.com/tweag/ormolu/issues/1032

{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_)
import IntegrationUtils (getFourmoluExe, readProcess)
import System.Directory (copyFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

main :: IO ()
main = hspec $
  describe "fixity-tests" . beforeAll getFourmoluExe $
    forM_ tests $ \Test {..} ->
      specify testLabel $ \fourmoluExe ->
        withSystemTempDirectory "fixity-test-dir" $ \tmpdir -> do
          let configFile = tmpdir ++ "/fourmolu.yaml"
              srcFile = tmpdir ++ "/input.hs"

          copyFile ("fixity-tests/" ++ testInputFileName) srcFile

          -- extend fourmolu.yaml to keep same formatting as Ormolu
          rootConfig <- readFile "fourmolu.yaml"
          writeFile configFile $
            if testUseConfig
              then
                unlines
                  [ rootConfig,
                    -- add information in the original `fixity-tests/.ormolu` file
                    "fixities:",
                    "- 'infixr 8 .='",
                    "- 'infixr 5 #, :>'",
                    "reexports:",
                    "- 'module Foo exports Control.Lens'"
                  ]
              else rootConfig

          actual <-
            readProcess fourmoluExe . concat $
              [ [srcFile, "--check-idempotence"],
                testArgs,
                if testUseConfig then [] else ["--no-cabal"]
              ]
          expected <- readFile $ "fixity-tests/" ++ testExpectedFileName
          actual `shouldBe` expected

data Test = Test
  { testLabel :: String,
    testInputFileName :: FilePath,
    testArgs :: [String],
    testUseConfig :: Bool,
    testExpectedFileName :: FilePath
  }

tests :: [Test]
tests =
  [ Test
      { testLabel = "File #0 works with no extra info",
        testInputFileName = "test-0-input.hs",
        testArgs = [],
        testUseConfig = False,
        testExpectedFileName = "test-0-no-extra-info-expected.hs"
      },
    Test
      { testLabel = "File #0 works with manual fixity info",
        testInputFileName = "test-0-input.hs",
        testArgs = ["--fixity", "infixr 8 .=", "--fixity", "infixr 5 :>"],
        testUseConfig = False,
        testExpectedFileName = "test-0-with-fixity-info-expected.hs"
      },
    Test
      { testLabel = "File #0 works with fixity info from config",
        testInputFileName = "test-0-input.hs",
        testArgs = ["--package", "base"],
        testUseConfig = True,
        testExpectedFileName = "test-0-with-fixity-info-expected.hs"
      },
    Test
      { testLabel = "File #1 works with no extra info",
        testInputFileName = "test-1-input.hs",
        testArgs = [],
        testUseConfig = False,
        testExpectedFileName = "test-1-no-extra-info-expected.hs"
      },
    Test
      { testLabel = "File #1 works with manual fixity info",
        testInputFileName = "test-1-input.hs",
        testArgs = ["--fixity", "infixr 8 .=", "--fixity", "infixr 5 #"],
        testUseConfig = False,
        testExpectedFileName = "test-1-with-fixity-info-expected.hs"
      },
    Test
      { testLabel = "File #1 works with fixity info from config",
        testInputFileName = "test-1-input.hs",
        testArgs = ["--package", "base"],
        testUseConfig = True,
        testExpectedFileName = "test-1-with-fixity-info-expected.hs"
      },
    Test
      { testLabel = "File #1 works with weird fixity info overwrite",
        testInputFileName = "test-1-input.hs",
        testArgs = ["--package", "base", "--fixity", "infixr 5 $"],
        testUseConfig = True,
        testExpectedFileName = "test-1-with-fixity-info-weird-overwrite-expected.hs"
      },
    Test
      { testLabel = "File #2 works with no extra info",
        testInputFileName = "test-2-input.hs",
        testArgs = ["--package", "base", "--package", "lens"],
        testUseConfig = False,
        testExpectedFileName = "test-2-no-extra-info-expected.hs"
      },
    Test
      { testLabel = "File #2 works with manual reexport info",
        testInputFileName = "test-2-input.hs",
        testArgs = ["--package", "base", "--package", "lens", "--reexport", "module Foo exports Control.Lens"],
        testUseConfig = False,
        testExpectedFileName = "test-2-with-reexports-expected.hs"
      },
    Test
      { testLabel = "File #2 works with fixity info from config",
        testInputFileName = "test-2-input.hs",
        testArgs = ["--package", "base", "--package", "lens"],
        testUseConfig = True,
        testExpectedFileName = "test-2-with-reexports-expected.hs"
      }
  ]
