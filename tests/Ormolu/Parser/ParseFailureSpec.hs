{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Parser.ParseFailureSpec (spec) where

import Data.Text (Text)
import Ormolu
import Ormolu.Utils (showOutputable)
import System.FilePath
import Test.Hspec

spec :: Spec
spec = do
  "disabling-preserves-error-location.hs" `failsAt` "12:1"
  "line-pragma.hs" `failsAt` "4:47"
  "options-pragma.hs" `pragmaParseFailsAt` "1:1-30"

failsAt :: String -> String -> Spec
failsAt filename location =
  let filePath = baseDir </> filename
   in it (filename ++ " fails at " ++ location) $
        ormoluFile unexpectedPragmaOptions defaultConfig filePath
          `shouldThrow` \case
            OrmoluParsingFailed srcSpan _ ->
              showOutputable srcSpan == filePath ++ ":" ++ location
            _ -> False

pragmaParseFailsAt :: String -> String -> Spec
pragmaParseFailsAt filename location =
  let filePath = baseDir </> filename
   in it (filename ++ " fails at " ++ location) $
        ormoluFile failPragmaOptions defaultConfig filePath
          `shouldThrow` \case
            FourmoluPragmaOptsParsingFailed realSrcSpan _ ->
              showOutputable realSrcSpan == filePath ++ ":" ++ location
            _ -> False

baseDir :: FilePath
baseDir = "data" </> "parse-failures"

unexpectedPragmaOptions :: PrinterOptsTotal -> Text -> Either Text PrinterOptsTotal
unexpectedPragmaOptions _ _ = Left "Pragma options are not expected in the parse failure tests"

failPragmaOptions :: PrinterOptsTotal -> Text -> Either Text PrinterOptsTotal
failPragmaOptions _ _ = Left "Can't parse any pragma options"
