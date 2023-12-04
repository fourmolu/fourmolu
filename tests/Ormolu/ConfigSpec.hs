{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Fourmolu file configuration.
module Ormolu.ConfigSpec (spec) where

import Data.ByteString.Char8 qualified as Char8
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Yaml qualified as Yaml
import Ormolu.Config (FourmoluConfig (..), poIndentation, resolvePrinterOpts)
import Ormolu.Fixity (ModuleReexports (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "FourmoluConfig" $ do
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
    it "applies configurations in correct order" $ do
      let opts1 = mempty {poIndentation = Just 2}
          opts2 = mempty {poIndentation = Just 4}
          configs = [opts1, opts2]
      poIndentation (resolvePrinterOpts configs) `shouldBe` 4
