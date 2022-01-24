{-# LANGUAGE DeriveGeneric #-}

-- | This module only defines PrinterOpts, needed to be separate because of
-- Template Haskell staging restrictions.
module Ormolu.Config.Types
  ( PrinterOpts (..),
    CommaStyle (..),
    HaddockPrintStyle (..),
  )
where

import GHC.Generics (Generic)

-- | Options controlling formatting output.
data PrinterOpts f = PrinterOpts
  { -- | Number of spaces to use for indentation
    poIndentation :: f Int,
    -- | Whether to place commas at start or end of lines
    poCommaStyle :: f CommaStyle,
    -- | Whether to place commas at start or end of import-export lines
    poImportExportCommaStyle :: f CommaStyle,
    -- | Whether to indent `where` blocks
    poIndentWheres :: f Bool,
    -- | Leave space before opening record brace
    poRecordBraceSpace :: f Bool,
    -- | Trailing commas with parentheses on separate lines
    poDiffFriendlyImportExport :: f Bool,
    -- | Be less opinionated about spaces/newlines etc.
    poRespectful :: f Bool,
    -- | How to print doc comments
    poHaddockStyle :: f HaddockPrintStyle,
    -- | Number of newlines between top-level decls
    poNewlinesBetweenDecls :: f Int
  }
  deriving (Generic)

data CommaStyle
  = Leading
  | Trailing
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

data HaddockPrintStyle
  = HaddockSingleLine
  | HaddockMultiLine
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)
