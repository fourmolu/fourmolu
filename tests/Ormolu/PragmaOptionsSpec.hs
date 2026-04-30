{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.PragmaOptionsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Ormolu
import Ormolu.Config (ImportGrouping (..))
import Test.Hspec

spec :: Spec
spec = describe "Fourmolu options in source files take priority" $ do
  it "local printer configuration takes precedence over the application configuration" $ do
    let src =
          T.unlines
            [ "{- FOURMOLU_OPTIONS --indentation 6 --import-grouping by-qualified -}",
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
    fixedPoint (\cfg -> cfg {poIndentation = pure 4, poImportGrouping = pure ImportGroupLegacy}) src
  where
    fixedPoint basePrinterOpts input = do
      let cfg = defaultConfig {cfgPrinterOpts = basePrinterOpts (cfgPrinterOpts defaultConfig)}
      output <- ormolu testPragmaOptionsParser cfg "<input>" input
      output `shouldBe` input

-- The argument parsing function is in Main, so we can't use it here.
-- Duplicating it in the library code may not be desirable to ease upstream
-- sync. So we simulate its behavior in tests, so that the test input codes
-- remain realistic, though care is required when updating CLI options.
testPragmaOptionsParser :: PrinterOptsTotal -> Text -> Either Text PrinterOptsTotal
testPragmaOptionsParser cfg "--indentation 6 --import-grouping by-qualified" = Right $ cfg {poIndentation = pure 6, poImportGrouping = pure ImportGroupByQualified}
testPragmaOptionsParser _ _ = Left "Unidentified pragma test scenario"
