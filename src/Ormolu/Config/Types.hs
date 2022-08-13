{-# LANGUAGE DeriveGeneric #-}

-- | This module defines PrinterOpts and related types
module Ormolu.Config.Types
  ( PrinterOpts (..),
    CommaStyle (..),
    HaddockPrintStyle (..),
    ImportExportStyle (..),
  )
where

import GHC.Generics (Generic)

-- | Options controlling formatting output.
data PrinterOpts f = PrinterOpts
  { -- | Number of spaces to use for indentation
    poIndentation :: f Int,
    -- | Whether to place commas at start or end of lines
    poCommaStyle :: f CommaStyle,
    -- | Styling of import/export lists
    poImportExportStyle :: f ImportExportStyle,
    -- | Whether to indent `where` blocks
    poIndentWheres :: f Bool,
    -- | Leave space before opening record brace
    poRecordBraceSpace :: f Bool,
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
  deriving (Eq, Show, Enum, Bounded)

data HaddockPrintStyle
  = HaddockSingleLine
  | HaddockMultiLine
  | HaddockMultiLineCompact
  deriving (Eq, Show, Enum, Bounded)

data ImportExportStyle
  = ImportExportLeading
  | ImportExportTrailing
  | ImportExportDiffFriendly
  deriving (Eq, Show, Enum, Bounded)
