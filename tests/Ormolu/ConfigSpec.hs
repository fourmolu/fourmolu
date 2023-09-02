{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Fourmolu file configuration.
module Ormolu.ConfigSpec (spec) where

import Data.ByteString.Char8 qualified as Char8
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Ormolu.Config
import Ormolu.Fixity (ModuleReexports (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "FourmoluConfig" $ do
    it "resolves options with presets" $ do
      let cfg = mempty {poCommaStyle = pure Leading}

      let fourmoluPreset = resolvePrinterOpts [Just FourmoluPreset] [cfg]
      poIndentation fourmoluPreset `shouldBe` pure 4
      poCommaStyle fourmoluPreset `shouldBe` pure Leading

      let ormoluPreset = resolvePrinterOpts [Just OrmoluPreset] [cfg]
      poIndentation ormoluPreset `shouldBe` pure 2
      poCommaStyle ormoluPreset `shouldBe` pure Leading

      let noPreset = resolvePrinterOpts [Nothing] [cfg]
      noPreset `shouldBe` fourmoluPreset

    it "parses multiple reexports from same module" $ do
      config <-
        Yaml.decodeThrow . Char8.pack . unlines $
          [ "reexports:",
            "- module Foo exports Bar1",
            "- module Foo exports Bar2"
          ]
      let expected =
            Map.fromList
              [ ("Foo", NonEmpty.fromList [(Nothing, "Bar2"), (Nothing, "Bar1")])
              ]
      cfgFileReexports config `shouldBe` ModuleReexports expected
