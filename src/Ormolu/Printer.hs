{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printModule,
    PrinterOpts (..),
  )
where

import Data.Text (Text)
import Ormolu.Config
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream
import Ormolu.Processing.Postprocess (postprocess)

-- | Render a module.
printModule ::
  -- | Result of parsing
  ParseResult ->
  PrinterOpts ->
  -- | Resulting rendition
  Text
printModule ParseResult {..} printerOpts =
  prLiteralPrefix <> region <> prLiteralSuffix
  where
    region =
      postprocess $
        runR
          ( p_hsModule
              prStackHeader
              prShebangs
              prPragmas
              prImportQualifiedPost
              prParsedSource
          )
          (mkSpanStream prParsedSource)
          prCommentStream
          prAnns
          printerOpts
          prUseRecordDot
