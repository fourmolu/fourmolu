{-# LANGUAGE OverloadedStrings #-}

module Ormolu.PragmaOptionsSpec (spec) where

import Data.Text qualified as T
import Ormolu
import Ormolu.Config (ImportGrouping (..))
import Test.Hspec

spec :: Spec
spec = describe "Fourmolu options in source files take priority" $ do
  it "local printer configuration takes precedence over the application configuration" $ do
    let cfg =
          defaultConfig
            { cfgPrinterOpts =
                (cfgPrinterOpts defaultConfig)
                  { poIndentation = pure 4,
                    poImportGrouping = pure ImportGroupLegacy
                  }
            }
        input =
          T.unlines
            [ "{- FOURMOLU_OPTIONS { indentation: 6, import-grouping: by-qualified } -}",
              "",
              "import Data.Functor",
              "import Data.Maybe (maybe)",
              "import Data.Text (Text)",
              "import PseudoInternal.Monad",
              "import SomeInternal.Module1 (anotherDefinition, someDefinition)",
              "",
              "import qualified Data.Functor as Functor",
              "import qualified Data.Text",
              "",
              "hello = do",
              "      undefined"
            ]
    output <- ormolu cfg "<input>" input
    output `shouldBe` input
