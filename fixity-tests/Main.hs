{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, when)
import System.Directory (copyFile, findExecutable)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

main :: IO ()
main = hspec $
  describe "fixity-tests" . beforeAll getExe $
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
                    "fixities:",
                    "- 'infixr 8 .='",
                    "- 'infixr 5 #'"
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
        testArgs = ["--fixity", "infixr 8 .="],
        testUseConfig = False,
        testExpectedFileName = "test-0-with-fixity-info-expected.hs"
      },
    Test
      { testLabel = "File #0 works with fixity info from config",
        testInputFileName = "test-0-input.hs",
        testArgs = [],
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
        testArgs = [],
        testUseConfig = True,
        testExpectedFileName = "test-1-with-fixity-info-expected.hs"
      }
  ]

-- | Find a `fourmolu` executable on PATH.
getExe :: IO FilePath
getExe = findExecutable "fourmolu" >>= maybe (fail "Could not find fourmolu executable") return

readProcess :: FilePath -> [String] -> IO String
readProcess cmd args = do
  (code, stdout, stderr) <- readProcessWithExitCode cmd args ""
  when (code /= ExitSuccess) $ do
    putStrLn $ "Command failed: " ++ (unwords . map (\s -> "\"" ++ s ++ "\"")) (cmd : args)
    putStrLn "========== stdout =========="
    putStrLn stdout
    putStrLn "========== stderr =========="
    putStrLn stderr
    fail "Command failed. See output for more details."
  return stdout
