{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of import and export lists.
module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports,
    p_hsmodImport,
    breakIfNotDiffFriendly,
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC
import Ormolu.Config (poDiffFriendlyImportExport)
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
      (attachRelativePos xs)

p_hsmodImport :: Bool -> ImportDecl GhcPs -> R ()
p_hsmodImport useQualifiedPost ImportDecl {..} = do
  txt "import"
  space
  when ideclSource (txt "{-# SOURCE #-}")
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
      Just (hiding, L _ xs) -> do
        when hiding (txt "hiding")
        breakIfNotDiffFriendly
        parens' True $ do
          layout <- getLayout
          sep
            breakpoint
            (\(p, l) -> sitcc (located l (p_lie layout p)))
            (attachRelativePos xs)
    newline
p_hsmodImport _ (XImportDecl x) = noExtCon x

p_lie :: Layout -> RelativePos -> IE GhcPs -> R ()
p_lie encLayout relativePos = \case
  IEVar NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAbs NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAll NoExtField l1 -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
    p_comma
  IEThingWith NoExtField l1 w xs _ -> sitcc $ do
    located l1 p_ieWrappedName
    breakIfNotDiffFriendly
    inci $ do
      let names :: [R ()]
          names = located' p_ieWrappedName <$> xs
      parens' False . sep commaDel' sitcc $
        case w of
          NoIEWildcard -> names
          IEWildcard n ->
            let (before, after) = splitAt n names
             in before ++ [txt ".."] ++ after
    p_comma
  IEModuleContents NoExtField l1 -> do
    located l1 p_hsmodName
    p_comma
  IEGroup NoExtField n str -> do
    case relativePos of
      SinglePos -> return ()
      FirstPos -> return ()
      MiddlePos -> newline
      LastPos -> newline
    p_hsDocString (Asterisk n) False (noLoc str)
  IEDoc NoExtField str ->
    p_hsDocString Pipe False (noLoc str)
  IEDocNamed NoExtField str -> txt $ "-- $" <> T.pack str
  XIE x -> noExtCon x
  where
    p_comma =
      case encLayout of
        SingleLine ->
          case relativePos of
            SinglePos -> return ()
            FirstPos -> comma
            MiddlePos -> comma
            LastPos -> return ()
        MultiLine -> comma

----------------------------------------------------------------------------
-- Unlike the versions in 'Ormolu.Printer.Combinators', these do not depend on
-- whether 'leadingCommas' is set. This is useful here is we choose to keep
-- import and export lists independent of that setting.

-- | Delimiting combination with 'comma'. To be used with 'sep'.
commaDel' :: R ()
commaDel' = comma >> breakpoint

-- | Surround given entity by parentheses @(@ and @)@.
parens' :: Bool -> R () -> R ()
parens' topLevelImport m =
  getPrinterOpt poDiffFriendlyImportExport >>= \case
    True -> do
      txt "("
      breakpoint'
      sitcc body
      vlayout (txt ")") (inciBy (-1) trailingParen)
    False -> sitcc $ do
      txt "("
      body
      txt ")"
  where
    body = vlayout singleLine multiLine
    singleLine = m
    multiLine = do
      space
      sitcc m
      newline
    trailingParen = if topLevelImport then txt " )" else txt ")"

breakIfNotDiffFriendly :: R ()
breakIfNotDiffFriendly =
  getPrinterOpt poDiffFriendlyImportExport >>= \case
    True -> space
    False -> breakpoint
