module Ormolu.Integration.CLIOptionsSpec (spec) where

import Data.List (isPrefixOf)
import Ormolu.Integration.Utils (getFourmoluExe, readProcess)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec =
  describe "Fourmolu configuration via CLI" . beforeAll getFourmoluExe $ do
    it "CLI options override default" $ \fourmoluExe -> do
      withTempDir $ \tmpdir -> do
        let hsFile = tmpdir </> "test.hs"
        writeFile hsFile indented2

        withoutCLI <- readProcess fourmoluExe [hsFile]
        withoutCLI `shouldBe` indented4

        withCLI <- readProcess fourmoluExe ["--indentation=2", hsFile]
        withCLI `shouldBe` indented2

    it "CLI options used when config file lacks option" $ \fourmoluExe -> do
      withTempDir $ \tmpdir -> do
        let hsFile = tmpdir </> "test.hs"
        writeFile hsFile indented2
        let configFile = tmpdir </> "fourmolu.yaml"
        writeFile configFile "comma-style: trailing"

        withoutCLI <- readProcess fourmoluExe [hsFile]
        withoutCLI `shouldBe` indented4

        withCLI <- readProcess fourmoluExe ["--indentation=2", hsFile]
        withCLI `shouldBe` indented2

    it "CLI options override config file option" $ \fourmoluExe -> do
      withTempDir $ \tmpdir -> do
        let hsFile = tmpdir </> "test.hs"
        writeFile hsFile indented2
        let configFile = tmpdir </> "fourmolu.yaml"
        writeFile configFile "indentation: 2"

        withoutCLI <- readProcess fourmoluExe [hsFile]
        withoutCLI `shouldBe` indented2

        withCLI <- readProcess fourmoluExe ["--indentation=4", hsFile]
        withCLI `shouldBe` indented4

    it "prints defaults to stdout" $ \fourmoluExe -> do
      stdOutput <- readProcess fourmoluExe ["--print-defaults"]
      -- Only check prefix of the output, so we don't have to update the test with every new option added
      stdOutput `shouldSatisfy` isPrefixOf "# Number of spaces per indentation step\nindentation: 4\n"
  where
    withTempDir = withSystemTempDirectory "fourmolu-cli-options-test"
    indented2 =
      unlines
        [ "main :: IO ()",
          "main =",
          "  print 10"
        ]
    indented4 =
      unlines
        [ "main :: IO ()",
          "main =",
          "    print 10"
        ]
