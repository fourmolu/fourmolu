{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printSnippets,
    PrinterOpts (..),
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Ormolu.Config
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream
import Ormolu.Processing.Common

-- | Render several source snippets.
printSnippets ::
  -- | Whether to print out debug information during printing
  Bool ->
  -- | Result of parsing
  [SourceSnippet] ->
  PrinterOptsTotal ->
  -- | Resulting rendition
  Text
printSnippets debug snippets printerOpts = T.concat . fmap printSnippet $ snippets
  where
    printSnippet = \case
      ParsedSnippet ParseResult {..} ->
        reindent prIndent $
          runR
            ( p_hsModule
                prStackHeader
                prPragmas
                prParsedSource
            )
            (mkSpanStream prParsedSource)
            prCommentStream
            printerOpts
            prLocalModules
            prSourceType
            prExtensions
            prModuleFixityMap
            debug
      RawSnippet r -> r
