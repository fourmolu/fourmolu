{-# LANGUAGE DeriveGeneric #-}

-- | This module defines PrinterOpts and related types
module Ormolu.Config.Types
  ( PrinterOpts (..),
    CommaStyle (..),
    FunctionArrowsStyle (..),
    HaddockPrintStyle (..),
    HaddockPrintStyleModule (..),
    ImportExportStyle (..),
    LetStyle (..),
    InStyle (..),
    Unicode (..),
  )
where

import GHC.Generics (Generic)

-- | Options controlling formatting output.
data PrinterOpts f = PrinterOpts
  { -- | Number of spaces to use for indentation
    poIndentation :: f Int,
    -- | How to style arrows in type signatures
    poFunctionArrows :: f FunctionArrowsStyle,
    -- | Whether to place commas at start or end of lines
    poCommaStyle :: f CommaStyle,
    -- | Styling of import/export lists
    poImportExportStyle :: f ImportExportStyle,
    -- | Whether to indent `where` blocks
    poIndentWheres :: f Bool,
    -- | Leave space before opening record brace
    poRecordBraceSpace :: f Bool,
    -- | Number of newlines between top-level decls
    poNewlinesBetweenDecls :: f Int,
    -- | How to print doc comments
    poHaddockStyle :: f HaddockPrintStyle,
    -- | How to print the module docstring (defaults to poHaddockStyle)
    poHaddockStyleModule :: f HaddockPrintStyleModule,
    -- | Styling of let blocks
    poLetStyle :: f LetStyle,
    -- | How to align in keyword
    poInStyle :: f InStyle,
    -- | Output Unicode syntax
    poUnicode :: f Unicode,
    -- | Be less opinionated about spaces/newlines etc.
    poRespectful :: f Bool
  }
  deriving (Generic)

data CommaStyle
  = Leading
  | Trailing
  deriving (Eq, Show, Enum, Bounded)

data FunctionArrowsStyle
  = TrailingArrows
  | LeadingArrows
  deriving (Eq, Show, Enum, Bounded)

data HaddockPrintStyle
  = HaddockSingleLine
  | HaddockMultiLine
  | HaddockMultiLineCompact
  deriving (Eq, Show, Enum, Bounded)

data HaddockPrintStyleModule
  = PrintStyleInherit
  | PrintStyleOverride HaddockPrintStyle
  deriving (Eq, Show)

data ImportExportStyle
  = ImportExportLeading
  | ImportExportTrailing
  | ImportExportDiffFriendly
  deriving (Eq, Show, Enum, Bounded)

data LetStyle
  = LetAuto
  | LetInline
  | LetNewline
  | LetMixed
  deriving (Eq, Show, Enum, Bounded)

data InStyle
  = InLeftAlign
  | InRightAlign
  deriving (Eq, Show, Enum, Bounded)

data Unicode
  = UnicodeDetect
  | UnicodeAlways
  | UnicodeNever
  deriving (Eq, Show, Enum, Bounded)
