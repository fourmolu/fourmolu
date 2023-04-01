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

    it "prints defaults to stdout" $ \fourmoluExe -> do
      readProcess fourmoluExe ["--print-defaults"]
        `shouldReturn` "indentation: 4 # Number of spaces per indentation step\n\
                       \function-arrows: trailing # Styling of arrows in type signatures (choices: \"trailing\", \"leading\", or \"leading-args\")\n\
                       \comma-style: leading # How to place commas in multi-line lists, records, etc. (choices: \"leading\" or \"trailing\")\n\
                       \import-export-style: diff-friendly # Styling of import/export lists (choices: \"leading\", \"trailing\", or \"diff-friendly\")\n\
                       \indent-wheres: false # Whether to full-indent or half-indent 'where' bindings past the preceding body\n\
                       \record-brace-space: false # Whether to leave a space before an opening record brace\n\
                       \newlines-between-decls: 1 # Number of spaces between top-level declarations\n\
                       \haddock-style: multi-line # How to print Haddock comments (choices: \"single-line\", \"multi-line\", or \"multi-line-compact\")\n\
                       \haddock-style-module: null # How to print module docstring\n\
                       \let-style: auto # Styling of let blocks (choices: \"auto\", \"inline\", \"newline\", or \"mixed\")\n\
                       \in-style: right-align # How to align the 'in' keyword with respect to the 'let' keyword (choices: \"left-align\", \"right-align\", or \"no-space\")\n\
                       \unicode: never # Output Unicode syntax (choices: \"detect\", \"always\", or \"never\")\n\
                       \respectful: true # Give the programmer more choice on where to insert blank lines\n\
                       \fixities: [] # Fixity information for operators\n\
                       \single-constraint-parens: always # Whether to put parentheses around a single constraint (choices: \"auto\", \"always\", or \"never\")\n\
                       \column-limit: none # Max line length for automatic line breaking\n\n"
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
