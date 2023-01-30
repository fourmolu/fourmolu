{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Printing combinators. The definitions here are presented in such an
-- order so you can just go through the Haddocks and by the end of the file
-- you should have a pretty good idea how to program rendering logic.
module Ormolu.Printer.Combinators
  ( -- * The 'R' monad
    R,
    runR,
    getEnclosingSpan,
    isExtensionEnabled,

    -- * Combinators

    -- ** Basic
    txt,
    atom,
    space,
    newline,
    declNewline,
    inci,
    inciBy,
    inciIf,
    inciByFrac,
    inciHalf,
    askSourceType,
    askFixityOverrides,
    askFixityMap,
    located,
    located',
    switchLayout,
    Layout (..),
    vlayout,
    getLayout,
    breakpoint,
    breakpoint',
    getPrinterOpt,

    -- ** Formatting lists
    sep,
    sepSemi,
    canUseBraces,
    useBraces,
    dontUseBraces,

    -- ** Wrapping
    BracketStyle (..),
    sitcc,
    backticks,
    banana,
    braces,
    brackets,
    parens,
    parensHash,
    pragmaBraces,
    pragma,

    -- ** Literals
    comma,
    commaDel,
    commaDelImportExport,
    equals,
    token'Larrowtail,
    token'Rarrowtail,
    token'darrow,
    token'dcolon,
    token'larrow,
    token'larrowtail,
    token'rarrow,
    token'rarrowtail,
    token'star,
    token'forall,
    token'oparenbar,
    token'cparenbar,
    token'openExpQuote,
    token'closeQuote,
    token'lolly,

    -- ** Stateful markers
    SpanMark (..),
    spanMarkSpan,
    HaddockStyle (..),
    setSpanMark,
    getSpanMark,

    -- ** Placement
    Placement (..),
    placeHanging,
  )
where

import Control.Monad
import Data.List (intersperse)
import Data.Text (Text)
import qualified GHC.Data.Strict as Strict
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc
import Ormolu.Config
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal
import Ormolu.Utils (HasSrcSpan (..))

----------------------------------------------------------------------------
-- Basic

-- | Indent the inner expression if the first argument is 'True'.
inciIf ::
  -- | Whether to indent
  Bool ->
  -- | The expression to indent
  R () ->
  R ()
inciIf b m = if b then inci m else m

-- | Enter a 'GenLocated' entity. This combinator handles outputting comments
-- and sets layout (single-line vs multi-line) for the inner computation.
-- Roughly, the rule for using 'located' is that every time there is a
-- 'Located' wrapper, it should be “discharged” with a corresponding
-- 'located' invocation.
located ::
  (HasSrcSpan l) =>
  -- | Thing to enter
  GenLocated l a ->
  -- | How to render inner value
  (a -> R ()) ->
  R ()
located (L l' a) f = case loc' l' of
  UnhelpfulSpan _ -> f a
  RealSrcSpan l _ -> do
    spitPrecedingComments l
    withEnclosingSpan l $
      switchLayout [RealSrcSpan l Strict.Nothing] (f a)
    spitFollowingComments l

-- | A version of 'located' with arguments flipped.
located' ::
  (HasSrcSpan l) =>
  -- | How to render inner value
  (a -> R ()) ->
  -- | Thing to enter
  GenLocated l a ->
  R ()
located' = flip located

-- | Set layout according to combination of given 'SrcSpan's for a given.
-- Use this only when you need to set layout based on e.g. combined span of
-- several elements when there is no corresponding 'Located' wrapper
-- provided by GHC AST. It is relatively rare that this one is needed.
--
-- Given empty list this function will set layout to single line.
switchLayout ::
  -- | Span that controls layout
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayout spans' = enterLayout (spansLayout spans')

-- | Which layout combined spans result in?
spansLayout :: [SrcSpan] -> Layout
spansLayout = \case
  [] -> SingleLine
  (x : xs) ->
    if isOneLineSpan (foldr combineSrcSpans x xs)
      then SingleLine
      else MultiLine

-- | Insert a space if enclosing layout is single-line, or newline if it's
-- multiline.
--
-- > breakpoint = vlayout space newline
breakpoint :: R ()
breakpoint = vlayout space newline

-- | Similar to 'breakpoint' but outputs nothing in case of single-line
-- layout.
--
-- > breakpoint' = vlayout (return ()) newline
breakpoint' :: R ()
breakpoint' = vlayout (return ()) newline

----------------------------------------------------------------------------
-- Formatting lists

-- | Render a collection of elements inserting a separator between them.
sep ::
  -- | Separator
  R () ->
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sep s f xs = sequence_ (intersperse s (f <$> xs))

-- | Render a collection of elements layout-sensitively using given printer,
-- inserting semicolons if necessary and respecting 'useBraces' and
-- 'dontUseBraces' combinators.
--
-- > useBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "{ foo; bar }") (txt "foo\nbar")
--
-- > dontUseBraces $ sepSemi txt ["foo", "bar"]
-- >   == vlayout (txt "foo; bar") (txt "foo\nbar")
sepSemi ::
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sepSemi f xs = vlayout singleLine multiLine
  where
    singleLine = do
      ub <- canUseBraces
      case xs of
        [] -> when ub $ txt "{}"
        xs' ->
          if ub
            then do
              txt "{"
              space
              sep (txt ";" >> space) (dontUseBraces . f) xs'
              space
              txt "}"
            else sep (txt ";" >> space) f xs'
    multiLine =
      sep newline (dontUseBraces . f) xs

----------------------------------------------------------------------------
-- Wrapping

-- | 'BracketStyle' controlling how closing bracket is rendered.
data BracketStyle
  = -- | Normal
    N
  | -- | Shifted one level
    S
  deriving (Eq, Show)

-- | Surround given entity by backticks.
backticks :: R () -> R ()
backticks m = do
  txt "`"
  m
  txt "`"

-- | Surround given entity by banana brackets (i.e., from arrow notation.)
banana :: BracketStyle -> R () -> R ()
banana = brackets_ True token'oparenbar token'cparenbar

-- | Surround given entity by curly braces @{@ and  @}@.
braces :: BracketStyle -> R () -> R ()
braces = brackets_ False (txt "{") (txt "}")

-- | Surround given entity by square brackets @[@ and @]@.
brackets :: BracketStyle -> R () -> R ()
brackets = brackets_ False (txt "[") (txt "]")

-- | Surround given entity by parentheses @(@ and @)@.
parens :: BracketStyle -> R () -> R ()
parens = brackets_ False (txt "(") (txt ")")

-- | Surround given entity by @(# @ and @ #)@.
parensHash :: BracketStyle -> R () -> R ()
parensHash = brackets_ True (txt "(#") (txt "#)")

-- | Braces as used for pragmas: @{\-#@ and @#-\}@.
pragmaBraces :: R () -> R ()
pragmaBraces m = sitcc $ do
  txt "{-#"
  space
  m
  breakpoint
  inci (txt "#-}")

-- | Surround the body with a pragma name and 'pragmaBraces'.
pragma ::
  -- | Pragma text
  Text ->
  -- | Pragma body
  R () ->
  R ()
pragma pragmaText body = pragmaBraces $ do
  txt pragmaText
  breakpoint
  body

-- | A helper for defining wrappers like 'parens' and 'braces'.
brackets_ ::
  -- | Insert breakpoints around brackets
  Bool ->
  -- | Opening bracket
  R () ->
  -- | Closing bracket
  R () ->
  -- | Bracket style
  BracketStyle ->
  -- | Inner expression
  R () ->
  R ()
brackets_ needBreaks open close style m = sitcc (vlayout singleLine multiLine)
  where
    singleLine = do
      open
      when needBreaks space
      m
      when needBreaks space
      close
    multiLine = do
      open
      commaStyle <- getPrinterOpt poCommaStyle
      case commaStyle of
        Leading ->
          if needBreaks
            then inci $ newline >> m
            else inciIf (style == S) $ space >> m
        Trailing ->
          if needBreaks
            then newline >> inci m
            else space >> sitcc m
      newline
      inciIf (style == S) close

----------------------------------------------------------------------------
-- Literals

-- | Print @,@.
comma :: R ()
comma = txt ","

-- | Delimiting combination with 'comma'. To be used with 'sep'.
commaDel :: R ()
commaDel = getPrinterOpt poCommaStyle >>= commaDel'

-- | Delimiting combination with 'comma' for import-export lists.
-- To be used with `sep`.
commaDelImportExport :: R ()
commaDelImportExport =
  getPrinterOpt poImportExportStyle >>= \case
    ImportExportLeading -> commaDel' Leading
    ImportExportTrailing -> commaDel' Trailing
    ImportExportDiffFriendly -> commaDel' Trailing

commaDel' :: CommaStyle -> R ()
commaDel' = \case
  Leading -> breakpoint' >> comma >> space
  Trailing -> comma >> breakpoint

-- | Print @=@. Do not use @'txt' "="@.
equals :: R ()
equals = interferingTxt "="

----------------------------------------------------------------------------
-- Token literals
-- The names of the following literals are from GHC's
-- @compiler/GHC/Parser/Lexer.x@.

-- | Print @⤛@ or @-<<@ as appropriate.
token'Larrowtail :: R ()
token'Larrowtail = "⤛" `whenUnicodeOtherwise` "-<<"

-- | Print @⤜@ or @>>-@ as appropriate.
token'Rarrowtail :: R ()
token'Rarrowtail = "⤜" `whenUnicodeOtherwise` ">>-"

-- | Print @⇒@ or @=>@ as appropriate.
token'darrow :: R ()
token'darrow = "⇒" `whenUnicodeOtherwise` "=>"

-- | Print @∷@ or @::@ as appropriate.
token'dcolon :: R ()
token'dcolon = "∷" `whenUnicodeOtherwise` "::"

-- | Print @←@ or @<-@ as appropriate.
token'larrow :: R ()
token'larrow = "←" `whenUnicodeOtherwise` "<-"

-- | Print @⤙@ or @-<@ as appropriate.
token'larrowtail :: R ()
token'larrowtail = "⤙" `whenUnicodeOtherwise` "-<"

-- | Print @→@ or @->@ as appropriate.
token'rarrow :: R ()
token'rarrow = "→" `whenUnicodeOtherwise` "->"

-- | Print @⤚@ or @>-@ as appropriate.
token'rarrowtail :: R ()
token'rarrowtail = "⤚" `whenUnicodeOtherwise` ">-"

-- | Print @★@ or @*@ as appropriate.
token'star :: R ()
token'star = "★" `whenUnicodeOtherwise` "*"

-- | Print @∀@ or @forall@ as appropriate.
token'forall :: R ()
token'forall = "∀" `whenUnicodeOtherwise` "forall"

-- | Print @⦇@ or @(|@ as appropriate.
token'oparenbar :: R ()
token'oparenbar = "⦇" `whenUnicodeOtherwise` "(|"

-- | Print @⦈@ or @|)@ as appropriate.
token'cparenbar :: R ()
token'cparenbar = "⦈" `whenUnicodeOtherwise` "|)"

-- | Print @⟦@ or @[|@ as appropriate.
token'openExpQuote :: R ()
token'openExpQuote = "⟦" `whenUnicodeOtherwise` "[|"

-- | Print @⟧@ or @|]@ as appropriate.
token'closeQuote :: R ()
token'closeQuote = "⟧" `whenUnicodeOtherwise` "|]"

-- | Print @⊸@ or @%1 ->@ as appropriate.
token'lolly :: R ()
token'lolly = "⊸" `whenUnicodeOtherwise` "%1 ->"

-- | Write the one text or the other depending on whether Unicode is enabled.
whenUnicodeOtherwise :: Text -> Text -> R ()
unicodeText `whenUnicodeOtherwise` asciiText = do
  unicodePrinterOption <- getPrinterOpt poUnicode
  unicodeExtensionIsEnabled <- isExtensionEnabled UnicodeSyntax
  txt $ case unicodePrinterOption of
    UnicodeDetect -> if unicodeExtensionIsEnabled then unicodeText else asciiText
    UnicodeAlways -> unicodeText
    UnicodeNever -> asciiText

----------------------------------------------------------------------------
-- Placement

-- | Expression placement. This marks the places where expressions that
-- implement handing forms may use them.
data Placement
  = -- | Multi-line layout should cause
    -- insertion of a newline and indentation
    -- bump
    Normal
  | -- | Expressions that have hanging form
    -- should use it and avoid bumping one level
    -- of indentation
    Hanging
  deriving (Eq, Show)

-- | Place a thing that may have a hanging form. This function handles how
-- to separate it from preceding expressions and whether to bump indentation
-- depending on what sort of expression we have.
placeHanging :: Placement -> R () -> R ()
placeHanging placement m =
  case placement of
    Hanging -> do
      space
      m
    Normal -> do
      breakpoint
      inci m
