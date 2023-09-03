module Ormolu.Integration.PresetsSpec (spec) where

import Ormolu.Integration.Utils (getFourmoluExe, readProcess')
import Test.Hspec

spec :: Spec
spec =
  describe "Fourmolu presets functionality" . beforeAll getFourmoluExe $ do
    it "can import presets from a URL" $ \fourmoluExe -> do
      let opts =
            [ "--preset",
              "https://raw.githubusercontent.com/fourmolu/fourmolu/v0.13.1.0/fourmolu.yaml",
              "--no-cabal"
            ]
          unformattedCode = "func Foo{} = ()\n"
          formattedCode = "func Foo {} = ()\n"

      stdout <- readProcess' fourmoluExe opts unformattedCode
      stdout `shouldBe` formattedCode
