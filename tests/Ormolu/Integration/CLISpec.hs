module Ormolu.Integration.CLISpec (spec) where

import Control.Monad (forM_)
import Ormolu.Integration.Utils (ProcessSpec (..), getFourmoluExe, proc, readFrom, readProcess)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec =
  describe "Fourmolu CLI functionality" . beforeAll getFourmoluExe $ do
    it "formats stdin when no files specified" $ \fourmoluExe -> do
      stdout <- readFrom $ (proc fourmoluExe ["--no-cabal"]) {procStdin = unformattedCode}
      stdout `shouldBe` formattedCode

    it "formats stdin when stdin specified" $ \fourmoluExe -> do
      stdout <- readFrom $ (proc fourmoluExe ["--no-cabal", "-"]) {procStdin = unformattedCode}
      stdout `shouldBe` formattedCode

    it "finds config file in current directory when formatting stdin" $ \fourmoluExe ->
      withTempDir $ \tmpdir -> do
        writeFile' (tmpdir </> "fourmolu.yaml") "single-constraint-parens: never"
        stdout <-
          readFrom $
            (proc fourmoluExe ["--no-cabal", "-"])
              { procStdin = "f :: (Show a) => a -> String",
                procCwd = Just tmpdir
              }
        stdout `shouldBe` "f :: Show a => a -> String\n"

    it "finds config file in ancestor directory when formatting stdin" $ \fourmoluExe ->
      withTempDir $ \tmpdir -> do
        writeFile' (tmpdir </> "fourmolu.yaml") "single-constraint-parens: never"
        let cwd = tmpdir </> "a" </> "b" </> "c"
        createDirectoryIfMissing True cwd
        stdout <-
          readFrom $
            (proc fourmoluExe ["--no-cabal", "-"])
              { procStdin = "f :: (Show a) => a -> String",
                procCwd = Just cwd
              }
        stdout `shouldBe` "f :: Show a => a -> String\n"

    it "finds config file in directory of input file" $ \fourmoluExe ->
      withTempDir $ \tmpdir -> do
        writeFile' (tmpdir </> "fourmolu.yaml") "single-constraint-parens: never"
        writeFile' (tmpdir </> "input.hs") "f :: (Show a) => a -> String"
        stdout <-
          readFrom $
            (proc fourmoluExe ["--no-cabal", "input.hs"])
              { procCwd = Just tmpdir
              }
        stdout `shouldBe` "f :: Show a => a -> String\n"

    it "finds config file in XDG directory" $ \fourmoluExe ->
      withTempDir $ \tmpdir -> do
        let configDir = tmpdir </> "config"
        writeFile' (configDir </> "fourmolu.yaml") "single-constraint-parens: never"
        stdout <-
          readFrom $
            (proc fourmoluExe ["--no-cabal", "-"])
              { procStdin = "f :: (Show a) => a -> String",
                procCwd = Just tmpdir,
                procExtraEnv = [("XDG_CONFIG_HOME", configDir)]
              }
        stdout `shouldBe` "f :: Show a => a -> String\n"

    it "recursively finds files in directories" $ \fourmoluExe -> do
      withTempDir $ \tmpdir -> do
        let hsFile = tmpdir </> "test.hs"
        writeFile' hsFile unformattedCode

        output <- readProcess fourmoluExe [tmpdir]
        output `shouldBe` formattedCode

    it "ignores files in specific directories" $ \fourmoluExe -> do
      withTempDir $ \tmpdir -> do
        let testFiles =
              [ tmpdir </> "test1.hs",
                tmpdir </> "foo/test2.hs"
              ]
        let ignoredFiles =
              [ tmpdir </> ".stack-work/test.hs",
                tmpdir </> "dist-newstyle/test.hs"
              ]
        forM_ (testFiles ++ ignoredFiles) $ \fp -> do
          writeFile' fp unformattedCode

        _ <- readProcess fourmoluExe ["-i", tmpdir]
        forM_ testFiles $ \fp -> do
          output <- readFile fp
          output `shouldBe` formattedCode
        forM_ ignoredFiles $ \fp -> do
          output <- readFile fp
          output `shouldBe` unformattedCode
  where
    withTempDir = withSystemTempDirectory "fourmolu-cli-test"
    writeFile' fp s = do
      createDirectoryIfMissing True (takeDirectory fp)
      writeFile fp s
    unformattedCode = "main=print(1+1)\n"
    formattedCode = "main = print (1 + 1)\n"
