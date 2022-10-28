{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of import and export lists.
module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports,
    p_hsmodImport,
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc
import GHC.Unit.Types
import Ormolu.Config
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils (RelativePos (..), attachRelativePos)

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports [] = do
  txt "("
  breakpoint'
  txt ")"
p_hsmodExports xs =
  parens' False $ do
    layout <- getLayout
    sep
      breakpoint
      (\(p, l) -> sitcc (located l (p_lie layout p)))
      (attachRelativePos' xs)

p_hsmodImport :: ImportDecl GhcPs -> R ()
p_hsmodImport ImportDecl {..} = do
  useQualifiedPost <- isExtensionEnabled ImportQualifiedPost
  txt "import"
  space
  when (ideclSource == IsBoot) (txt "{-# SOURCE #-}")
  space
  when ideclSafe (txt "safe")
  space
  when
    (isImportDeclQualified ideclQualified && not useQualifiedPost)
    (txt "qualified")
  space
  case ideclPkgQual of
    Nothing -> return ()
    Just slit -> atom slit
  space
  inci $ do
    located ideclName atom
    when
      (isImportDeclQualified ideclQualified && useQualifiedPost)
      (space >> txt "qualified")
    case ideclAs of
      Nothing -> return ()
      Just l -> do
        space
        txt "as"
        space
        located l atom
    space
    case ideclHiding of
      Nothing -> return ()
      Just (hiding, _) ->
        when hiding (txt "hiding")
    case ideclHiding of
      Nothing -> return ()
      Just (_, L _ xs) -> do
        breakIfNotDiffFriendly
        parens' True $ do
          layout <- getLayout
          sep
            breakpoint
            (\(p, l) -> sitcc (located l (p_lie layout p)))
            (attachRelativePos xs)
    newline

p_lie :: Layout -> RelativePos -> IE GhcPs -> R ()
p_lie encLayout relativePos = \case
  IEVar NoExtField l1 ->
    withComma $
      located l1 p_ieWrappedName
  IEThingAbs _ l1 ->
    withComma $
      located l1 p_ieWrappedName
  IEThingAll _ l1 -> withComma $ do
    located l1 p_ieWrappedName
    space
    txt "(..)"
  IEThingWith _ l1 w xs -> sitcc . withComma $ do
    located l1 p_ieWrappedName
    breakIfNotDiffFriendly
    inci $ do
      let names :: [R ()]
          names = located' p_ieWrappedName <$> xs
      parens' False . sep commaDelImportExport sitcc $
        case w of
          NoIEWildcard -> names
          IEWildcard n ->
            let (before, after) = splitAt n names
             in before ++ [txt ".."] ++ after
  IEModuleContents _ l1 -> withComma $ do
    indentDoc $ located l1 p_hsmodName
  IEGroup NoExtField n str -> do
    case relativePos of
      SinglePos -> return ()
      FirstPos -> return ()
      MiddlePos -> newline
      LastPos -> newline
      FirstAfterDocPos -> newline
    indentDoc $ p_hsDocString (Asterisk n) False (noLoc str)
  IEDoc NoExtField str ->
    indentDoc $
      p_hsDocString Pipe False (noLoc str)
  IEDocNamed NoExtField str -> indentDoc $ txt $ "-- $" <> T.pack str
  where
    -- Add a comma to a import-export list element
    withComma m =
      case encLayout of
        SingleLine ->
          case relativePos of
            SinglePos -> void m
            FirstPos -> m >> comma
            MiddlePos -> m >> comma
            LastPos -> void m
            FirstAfterDocPos -> m >> comma
        MultiLine -> do
          commaStyle <- getCommaStyle
          case commaStyle of
            Leading ->
              case relativePos of
                FirstPos -> m
                FirstAfterDocPos -> inciBy 2 m
                SinglePos -> m
                _ -> comma >> space >> m
            Trailing -> m >> comma
    indentDoc m = do
      commaStyle <- getCommaStyle
      case commaStyle of
        Trailing -> m
        Leading ->
          case relativePos of
            SinglePos -> m
            FirstPos -> m
            _ -> inciBy 2 m

----------------------------------------------------------------------------

-- | Unlike the version in `Ormolu.Utils`, this version handles explicitly leading export documentation
attachRelativePos' :: [LIE GhcPs] -> [(RelativePos, LIE GhcPs)]
attachRelativePos' = \case
  [] -> []
  [x] -> [(SinglePos, x)]
  -- Check if leading export is a Doc
  (x@(L _ IEDoc {}) : xs) -> (FirstPos, x) : markDoc xs
  (x@(L _ IEGroup {}) : xs) -> (FirstPos, x) : markDoc xs
  (x@(L _ IEDocNamed {}) : xs) -> (FirstPos, x) : markDoc xs
  (x : xs) -> (FirstPos, x) : markLast xs
  where
    -- Mark leading documentation, making sure the first export gets assigned
    -- a `FirstPos`
    markDoc [] = []
    markDoc [x] = [(LastPos, x)]
    markDoc (x@(L _ IEDoc {}) : xs) = (FirstAfterDocPos, x) : markDoc xs
    markDoc (x@(L _ IEGroup {}) : xs) = (FirstAfterDocPos, x) : markDoc xs
    markDoc (x@(L _ IEDocNamed {}) : xs) = (FirstAfterDocPos, x) : markDoc xs
    -- First export after a Doc gets assigned a `FirstPos`
    markDoc (x : xs) = (FirstAfterDocPos, x) : markLast xs

    markLast [] = []
    markLast [x] = [(LastPos, x)]
    markLast (x : xs) = (MiddlePos, x) : markLast xs

-- | Surround given entity by parentheses @(@ and @)@.
parens' :: Bool -> R () -> R ()
parens' topLevelImport m =
  getPrinterOpt poImportExportStyle >>= \case
    ImportExportDiffFriendly -> do
      txt "("
      breakpoint'
      sitcc body
      vlayout (txt ")") (inciByFrac (-1) trailingParen)
    _ -> sitcc $ do
      txt "("
      body
      txt ")"
  where
    body = vlayout singleLine multiLine
    singleLine = m
    multiLine = do
      commaStyle <- getCommaStyle
      case commaStyle of
        -- On leading commas, list elements are inline with the enclosing parentheses
        Leading -> do
          space
          m
          newline
        -- On trailing commas, list elements are indented
        Trailing -> do
          space
          sitcc m
          newline
    trailingParen = if topLevelImport then txt " )" else txt ")"

getCommaStyle :: R CommaStyle
getCommaStyle =
  getPrinterOpt poImportExportStyle >>= \case
    ImportExportLeading -> pure Leading
    ImportExportTrailing -> pure Trailing
    ImportExportDiffFriendly -> pure Trailing

breakIfNotDiffFriendly :: R ()
breakIfNotDiffFriendly =
  getPrinterOpt poImportExportStyle >>= \case
    ImportExportDiffFriendly -> space
    _ -> breakpoint
