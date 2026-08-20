{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Printing combinators. The definitions here are presented in such an
-- order that you can just read through the Haddocks, and by the end of the
-- file you should have a pretty good idea of how to program rendering logic.
module Ormolu.Printer.Combinators
  ( -- * The 'R' monad
    R,
    runR,
    getEnclosingSpan,
    getCommentsAnchoredWithin,
    getCommentsBefore,
    isExtensionEnabled,

    -- * Combinators

    -- ** Basic
    txt,
    txtStripIndent,
    atom,
    space,
    newline,
    declNewline,
    multilineCommentNewline,
    newlineLiteral,
    inci,
    inciBy,
    inciIf,
    inciByFrac,
    askSourceType,
    askModuleFixityMap,
    askDebug,
    located,
    locatedEmpty,
    located',
    switchLayout,
    switchLayoutWithEnclosingComments,
    switchLayoutNoLimit,
    spansLayout,
    enterLayout,
    Layout (..),
    vlayout,
    getLayout,
    breakpoint,
    breakpoint',
    getPrinterOpt,
    getLocalModules,

    -- ** Formatting lists
    sep,
    sepSemi,
    sepSemi',
    canUseBraces,
    useBraces,
    dontUseBraces,

    -- ** Wrapping
    BracketStyle (..),
    sitcc,
    backticks,
    banana,
    braces,
    recordBraces,
    brackets,
    parens,
    parensHash,
    pragmaBraces,
    pragma,

    -- ** Literals
    comma,
    commaDel,
    commaDelImportExport,
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
    LastEmitted (..),
    lastEmittedSpan,
    HaddockStyle (..),
    setLastEmitted,
    getLastEmitted,

    -- ** Haddocks
    lookupHaddockText,

    -- ** Placement
    Placement (..),
    placeHanging,
  )
where

import Control.Monad
import Data.List (intersperse)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import GHC.Data.Strict qualified as Strict
import GHC.LanguageExtensions.Type
import GHC.Parser.Annotation hiding (IsUnicodeSyntax (..))
import GHC.Types.SrcLoc hiding (spans)
import Ormolu.Config
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal
import Ormolu.Utils (combineSrcSpans')

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
-- and sets the layout (single-line vs multi-line) for the inner computation.
-- Roughly, the rule for using 'located' is that every time there is a
-- 'Located' wrapper, it should be “discharged” with a corresponding
-- 'located' invocation.
located ::
  (HasLoc l) =>
  -- | Thing to enter
  GenLocated l a ->
  -- | How to render the inner value
  (a -> R ()) ->
  R ()
located (L l' a) f = case locA l' of
  UnhelpfulSpan _ -> f a
  RealSrcSpan l _ -> do
    recordVisitedSpan l
    spitPrecedingComments l
    withEnclosingSpan l $
      switchLayout [RealSrcSpan l Strict.Nothing] (f a)
    spitFollowingComments l

-- | Give an empty bracketed construct something for a comment written
-- inside it to attach to.
--
-- Brackets are rendered with 'txt', so an empty export or import list, an
-- empty @[]@ or a record with no fields contains no element at all. A
-- comment written between the brackets would be attached to whatever
-- encloses them and rendered outside them, so a zero-width element is
-- entered at the opening bracket instead.
locatedEmpty ::
  -- | Span of the empty construct
  SrcSpan ->
  R ()
locatedEmpty l =
  let loc = srcSpanStart l
   in located (L (mkSrcSpan loc loc) ()) pure

-- | A version of 'located' with the arguments flipped.
located' ::
  (HasLoc l) =>
  -- | How to render the inner value
  (a -> R ()) ->
  -- | Thing to enter
  GenLocated l a ->
  R ()
located' = flip located

-- | Set the layout according to the combination of the given 'SrcSpan's,
-- together with the spans of the comments that belong inside them.
--
-- Comments count towards the layout: a construct that would fit on one line
-- has to be broken up anyway if a comment was written inside it, or the
-- comment would swallow whatever follows it on the line.
--
-- 'located' calls this for you. Call it directly only when the layout has
-- to come from something the GHC AST has no 'Located' wrapper for, such as
-- the combined span of several elements; that is rare.
--
-- Given an empty list and no comments, this function will set the layout to
-- single-line.
switchLayout ::
  -- | Span that controls layout
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayout spans' m = do
  csSpans <- commentSpansIn (combineSrcSpans' <$> NE.nonEmpty spans')
  layout <- spansLayout (spans' <> csSpans)
  enterLayout layout m

-- | Same as 'switchLayout', except disregards the column limit.
--
-- It should be used for the argument list in function definitions because
-- the column limit can't be enforced there without changing the AST.
switchLayoutNoLimit :: [SrcSpan] -> R () -> R ()
switchLayoutNoLimit spans = enterLayout (spansLayoutWithLimit NoLimit spans)

-- | Like 'switchLayout', but the comments are looked for in the enclosing
-- element rather than in the given spans.
--
-- This is what a bracketed construct needs. In
--
-- > ( -- c
-- >   x
-- > )
--
-- the comment sits between the bracket and @x@, so it is inside neither of
-- them, and the parentheses would be put on one line despite it. Widening
-- the question to the enclosing element catches it. Do not reach for this
-- elsewhere: it is deliberately coarser than 'switchLayout', and applying
-- it where the enclosing element is large would let one comment break every
-- layout decision inside it.
switchLayoutWithEnclosingComments ::
  -- | Span that controls layout
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayoutWithEnclosingComments spans' m = do
  enclosing <- getEnclosingSpan
  csSpans <- commentSpansIn (flip RealSrcSpan Strict.Nothing <$> enclosing)
  layout <- spansLayout (spans' <> csSpans)
  enterLayout layout m

-- | The spans of the comments that belong inside the given region: both
-- attached to something in it and written inside it.
--
-- Both halves are needed. Without the first, a comment anywhere in a
-- declaration would force every layout decision inside that declaration to
-- multi-line. Without the second, a comment trailing an element would force
-- that element itself to be broken up.
--
-- Haddocks are not consulted here. They do not travel in the anchor map,
-- and their spans sit where the author wrote them rather than where they
-- will be printed, which is the wrong question; see
-- 'Ormolu.Printer.Meat.Common.multiLineIfDocumented'.
commentSpansIn :: Maybe SrcSpan -> R [SrcSpan]
commentSpansIn = \case
  Just (RealSrcSpan region _) -> do
    comments <- getCommentsAnchoredWithin region
    pure
      [ RealSrcSpan spn Strict.Nothing
      | L spn _ <- comments,
        region `containsSpan` spn
      ]
  _ -> pure []

-- | Which layout do the combined spans result in?
spansLayout :: [SrcSpan] -> R Layout
spansLayout spans = do
  colLimit <- getPrinterOpt poColumnLimit
  pure $ spansLayoutWithLimit colLimit spans

spansLayoutWithLimit :: ColumnLimit -> [SrcSpan] -> Layout
spansLayoutWithLimit colLimit = \case
  [] -> SingleLine
  (x : xs) ->
    let combinedSpan = foldr combineSrcSpans x xs
     in if isOneLineSpan combinedSpan && not (shouldBreakSingleLine combinedSpan)
          then SingleLine
          else MultiLine
  where
    shouldBreakSingleLine srcSpan =
      case (srcSpan, colLimit) of
        (RealSrcSpan rs _, ColumnLimit maxLineLength) ->
          let spanLineLength = srcSpanEndCol rs - srcSpanStartCol rs
           in spanLineLength > fromIntegral maxLineLength
        _ -> False

-- | Insert a space if the enclosing layout is single-line, or a newline if
-- it is multi-line.
--
-- > breakpoint = vlayout space newline
breakpoint :: R ()
breakpoint = vlayout space newline

-- | Similar to 'breakpoint', but outputs nothing in the case of single-line
-- layout.
--
-- > breakpoint' = vlayout (return ()) newline
breakpoint' :: R ()
breakpoint' = vlayout (return ()) newline

----------------------------------------------------------------------------
-- Formatting lists

-- | Render a collection of elements, inserting a separator between them.
sep ::
  -- | Separator
  R () ->
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sep s f xs = sequence_ (intersperse s (f <$> xs))

-- | Render a collection of elements layout-sensitively using the given
-- printer, inserting semicolons if necessary and respecting the 'useBraces'
-- and 'dontUseBraces' combinators.
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
sepSemi = sepSemi' False

-- | A version of 'sepSemi' that allows one to control whether semicolons
-- should be inserted in multi-line layout.
--
-- > useBraces $ sepSemi' False txt ["foo", "bar"]
-- >   == vlayout (txt "{ foo; bar }") (txt "foo\nbar")
--
-- > dontUseBraces $ sepSemi' True txt ["foo", "bar"]
-- >   == vlayout (txt "foo; bar") (txt "foo;\nbar")
sepSemi' ::
  -- | Whether to insert semicolons in multi-line layout
  Bool ->
  -- | How to render an element
  (a -> R ()) ->
  -- | Elements to render
  [a] ->
  R ()
sepSemi' addMultiColSemi f xs = vlayout singleLine multiLine
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
      sep
        (if addMultiColSemi then txt ";" >> newline else newline)
        (dontUseBraces . f)
        xs

----------------------------------------------------------------------------
-- Wrapping

-- | 'BracketStyle' controlling how the closing bracket is rendered.
data BracketStyle
  = -- | Normal
    N
  | -- | Shifted one level
    S
  deriving (Eq, Show)

-- | Surround the given entity with backticks.
backticks :: R () -> R ()
backticks m = do
  txt "`"
  m
  txt "`"

-- | Surround the given entity with banana brackets (i.e. from arrow
-- notation).
banana :: BracketStyle -> R () -> R ()
banana = brackets_ True token'oparenbar token'cparenbar

-- | Surround the given entity with curly braces @{@ and @}@.
braces :: BracketStyle -> R () -> R ()
braces = brackets_ False (txt "{") (txt "}")

-- | Surround the given entity with square brackets @[@ and @]@.
brackets :: BracketStyle -> R () -> R ()
brackets = brackets_ False (txt "[") (txt "]")

-- | Surround the given entity with parentheses @(@ and @)@.
parens :: BracketStyle -> R () -> R ()
parens = brackets_ False (txt "(") (txt ")")

-- | Surround the given entity with @(# @ and @ #)@.
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

-- With leading commas align the close brace with commas
-- otherwise move the close brace back to the left
recordBraces :: R () -> R ()
recordBraces m = do
  commaStyle <- getPrinterOpt poCommaStyle
  recordBraces_ (commaStyle == Trailing) m

recordBraces_ :: Bool -> R () -> R ()
recordBraces_ moveBraceBack m = do
  style <- getPrinterOpt poRecordStyle
  case style of
    RecordStyleAligned -> braces N m
    RecordStyleKnr ->
      vlayout
        (txt "{" >> m >> txt "}")
        ( do
            txt "{"
            newline
            sitcc m
            newline
            if moveBraceBack
              then inciByFrac (-1) (txt "}")
              else txt "}"
        )

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
-- support hanging forms may use them.
data Placement
  = -- | Multi-line layout should cause
    -- insertion of a newline and an
    -- indentation bump
    Normal
  | -- | Expressions that have a hanging form
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
