module Ormolu.Config.OptionsSpec (spec) where

import IntegrationUtils (getFourmoluExe, readProcess)
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
