{-# LANGUAGE RecordWildCards #-}

module Ormolu.CabalInfoSpec (spec) where

import Data.Set qualified as Set
import Distribution.Types.PackageName (unPackageName)
import Ormolu.Config (DynOption (..))
import Ormolu.Utils.Cabal
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "findCabalFile" $ do
    let findsOrmoluCabal start expectedCabalFile = do
          Just absolute <- findCabalFile start
          absolute `shouldSatisfy` isAbsolute
          makeRelativeToCurrentDirectory absolute `shouldReturn` expectedCabalFile
    it "returns correct absolute path" $
      findsOrmoluCabal "src/Ormolu/Config.hs" "fourmolu.cabal"
    it "finds correct path even when it starts from nonsense" $
      findsOrmoluCabal "a/b/c/d/e" "fourmolu.cabal"
    it "returns Nothing when it cannot find a cabal file" $
      findCabalFile "/foo.hs" `shouldReturn` Nothing
    it "does not consider directories as .cabal files" $
      withSystemTempDirectory "" $
        \dir -> do
          createDirectory $ dir </> ".cabal"
          cabalFile <- findCabalFile (dir </> "foo/bar.hs")
          cabalFile `shouldBe` Nothing
  describe "parseCabalInfo" $ do
    it "extracts correct cabal info from fourmolu.cabal for src/Ormolu/Config.hs" $ do
      (mentioned, CabalInfo {..}) <- parseCabalInfo "fourmolu.cabal" "src/Ormolu/Config.hs"
      mentioned `shouldBe` True
      unPackageName ciPackageName `shouldBe` "fourmolu"
      ciDynOpts `shouldBe` [DynOption "-XGHC2021"]
      Set.map unPackageName ciDependencies `shouldBe` Set.fromList ["Cabal-syntax", "Diff", "MemoTrie", "aeson", "ansi-terminal", "array", "base", "binary", "bytestring", "containers", "deepseq", "directory", "file-embed", "filepath", "ghc-lib-parser", "http-client", "http-client-tls", "megaparsec", "mtl", "network-uri", "scientific", "syb", "text", "yaml"]
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "fourmolu.cabal"
    it "extracts correct cabal info from fourmolu.cabal for tests/Ormolu/PrinterSpec.hs" $ do
      (mentioned, CabalInfo {..}) <- parseCabalInfo "fourmolu.cabal" "tests/Ormolu/PrinterSpec.hs"
      mentioned `shouldBe` True
      unPackageName ciPackageName `shouldBe` "fourmolu"
      ciDynOpts `shouldBe` [DynOption "-XGHC2021"]
      Set.map unPackageName ciDependencies `shouldBe` Set.fromList ["Cabal-syntax", "Diff", "QuickCheck", "base", "bytestring", "containers", "directory", "filepath", "ghc-lib-parser", "hspec", "hspec-megaparsec", "fourmolu", "megaparsec", "path", "path-io", "pretty", "process", "temporary", "text", "yaml"]
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "fourmolu.cabal"
    it "handles correctly files that are not mentioned in fourmolu.cabal" $ do
      (mentioned, CabalInfo {..}) <- parseCabalInfo "fourmolu.cabal" "src/FooBob.hs"
      mentioned `shouldBe` False
      unPackageName ciPackageName `shouldBe` "fourmolu"
      ciDynOpts `shouldBe` []
      Set.map unPackageName ciDependencies `shouldBe` Set.fromList ["base"]
      ciCabalFilePath `shouldSatisfy` isAbsolute
      makeRelativeToCurrentDirectory ciCabalFilePath `shouldReturn` "fourmolu.cabal"
    it "handles `hs-source-dirs: .`" $ do
      (_, CabalInfo {..}) <- parseTestCabalInfo "Foo.hs"
      ciDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
    it "handles empty hs-source-dirs" $ do
      (_, CabalInfo {..}) <- parseTestCabalInfo "Bar.hs"
      ciDynOpts `shouldContain` [DynOption "-XImportQualifiedPost"]
  where
    parseTestCabalInfo f =
      parseCabalInfo "data/cabal-tests/test.cabal" ("data/cabal-tests" </> f)
