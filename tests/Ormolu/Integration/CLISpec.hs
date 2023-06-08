module Ormolu.Integration.CLISpec (spec) where

import Control.Monad (forM_)
import Ormolu.Integration.Utils (getFourmoluExe, readProcess, readProcess')
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec =
  describe "Fourmolu CLI functionality" . beforeAll getFourmoluExe $ do
    it "formats stdin when no files specified" $ \fourmoluExe -> do
      stdout <- readProcess' fourmoluExe ["--no-cabal"] unformattedCode
      stdout `shouldBe` formattedCode

    it "formats stdin when stdin specified" $ \fourmoluExe -> do
      stdout <- readProcess' fourmoluExe ["--no-cabal", "-"] unformattedCode
      stdout `shouldBe` formattedCode

    it "recursively finds files in directories" $ \fourmoluExe -> do
      withTempDir $ \tmpdir -> do
        let hsFile = tmpdir </> "test.hs"
        writeFile hsFile unformattedCode

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
          createDirectoryIfMissing True (takeDirectory fp)
          writeFile fp unformattedCode

        _ <- readProcess fourmoluExe ["-i", tmpdir]
        forM_ testFiles $ \fp -> do
          output <- readFile fp
          output `shouldBe` formattedCode
        forM_ ignoredFiles $ \fp -> do
          output <- readFile fp
          output `shouldBe` unformattedCode
  where
    withTempDir = withSystemTempDirectory "fourmolu-cli-test"
    unformattedCode = "main=print(1+1)\n"
    formattedCode = "main = print (1 + 1)\n"
