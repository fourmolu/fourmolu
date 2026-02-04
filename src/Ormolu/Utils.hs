{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Random utilities used by the code.
module Ormolu.Utils
  ( RelativePos (..),
    attachRelativePos,
    combineSrcSpans',
    notImplemented,
    showOutputable,
    splitDocString,
    incSpanLine,
    numSeparatedByBlank,
    separatedByBlank,
    numSeparatedByBlankNE,
    separatedByBlankNE,
    onTheSameLine,
    groupBy',
    textToStringBuffer,
    ghcModuleNameToCabal,
  )
where

import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as TFFI
import Distribution.ModuleName (ModuleName)
import Distribution.ModuleName qualified as ModuleName
import Foreign (pokeElemOff, withForeignPtr)
import GHC.Data.Strict qualified as Strict
import GHC.Data.StringBuffer (StringBuffer (..))
import GHC.Driver.Ppr
import GHC.DynFlags (baseDynFlags)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.Hs hiding (ModuleName)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (Outputable (..))
import Language.Haskell.Syntax.Module.Name qualified as GHC

-- | Relative positions in a list.
data RelativePos
  = SinglePos
  | FirstPos
  | MiddlePos
  | LastPos
  deriving (Eq, Show)

-- | Attach 'RelativePos'es to elements of a given list.
attachRelativePos :: [a] -> [(RelativePos, a)]
attachRelativePos = \case
  [] -> []
  [x] -> [(SinglePos, x)]
  (x : xs) -> (FirstPos, x) : markLast xs
  where
    markLast [] = []
    markLast [x] = [(LastPos, x)]
    markLast (x : xs) = (MiddlePos, x) : markLast xs

-- | Combine all source spans from the given list.
combineSrcSpans' :: NonEmpty SrcSpan -> SrcSpan
combineSrcSpans' (x :| xs) = foldr combineSrcSpans x xs

-- | Placeholder for things that are not yet implemented.
notImplemented :: String -> a
notImplemented msg = error $ "not implemented yet: " ++ msg

-- | Pretty-print an 'GHC.Outputable' thing.
showOutputable :: (Outputable o) => o -> String
showOutputable = showSDoc baseDynFlags . ppr

-- | Split and normalize a doc string. The result is a list of lines that
-- make up the comment.
splitDocString :: Bool -> HsDocString -> [Text]
splitDocString shouldEscapeCommentBraces docStr =
  case r of
    [] -> [""]
    _ -> r
  where
    r =
      fmap (escapeLeadingDollar . escapeCommentBraces)
        . dropPaddingSpace'
        . dropWhileEnd T.null
        . fmap (T.stripEnd . T.pack)
        . lines
        $ renderHsDocString docStr
    -- We cannot have the first character to be a dollar because in that
    -- case it'll be a parse error (apparently collides with named docs
    -- syntax @-- $name@ somehow).
    escapeLeadingDollar txt =
      case T.uncons txt of
        Just ('$', _) -> T.cons '\\' txt
        _ -> txt
    dropPaddingSpace' =
      case docStr of
        -- comments using '--'
        MultiLineDocString {} -> dropPaddingSpace
        -- comments using '{-'
        NestedDocString {} -> \case
          x : xs | Just (' ', x') <- T.uncons x -> x' : xs
          xs -> xs
        -- don't care about generated
        GeneratedDocString {} -> id
    dropPaddingSpace xs =
      case dropWhile T.null xs of
        [] -> []
        (x : _) ->
          let leadingSpace txt = case T.uncons txt of
                Just (' ', _) -> True
                _ -> False
              dropSpace txt =
                if leadingSpace txt
                  then T.drop 1 txt
                  else txt
           in if leadingSpace x
                then dropSpace <$> xs
                else xs
    escapeCommentBraces =
      if shouldEscapeCommentBraces
        then T.replace "{-" "{\\-" . T.replace "-}" "-\\}"
        else id

-- | Increment line number in a 'SrcSpan'.
incSpanLine :: Int -> SrcSpan -> SrcSpan
incSpanLine i = \case
  RealSrcSpan s _ ->
    let start = realSrcSpanStart s
        end = realSrcSpanEnd s
        incLine x =
          let file = srcLocFile x
              line = srcLocLine x
              col = srcLocCol x
           in mkRealSrcLoc file (line + i) col
     in RealSrcSpan (mkRealSrcSpan (incLine start) (incLine end)) Strict.Nothing
  UnhelpfulSpan x -> UnhelpfulSpan x

-- | Number of blank lines separating two declarations
numSeparatedByBlank :: (a -> SrcSpan) -> a -> a -> Int
numSeparatedByBlank loc a b =
  fromMaybe 0 $ do
    endA <- srcSpanEndLine <$> srcSpanToRealSrcSpan (loc a)
    startB <- srcSpanStartLine <$> srcSpanToRealSrcSpan (loc b)
    pure (startB - endA - 1)

-- | Do two declarations have a blank between them?
separatedByBlank :: (a -> SrcSpan) -> a -> a -> Bool
separatedByBlank loc a b = numSeparatedByBlank loc a b >= 1

-- | Number of lines between two declaration groups
numSeparatedByBlankNE :: (a -> SrcSpan) -> NonEmpty a -> NonEmpty a -> Int
numSeparatedByBlankNE loc a b = numSeparatedByBlank loc (NE.last a) (NE.head b)

-- | Do two declaration groups have a blank between them?
separatedByBlankNE :: (a -> SrcSpan) -> NonEmpty a -> NonEmpty a -> Bool
separatedByBlankNE loc a b = separatedByBlank loc (NE.last a) (NE.head b)

-- | Return 'True' if one span ends on the same line the second one starts.
onTheSameLine :: SrcSpan -> SrcSpan -> Bool
onTheSameLine a b =
  isOneLineSpan (mkSrcSpan (srcSpanEnd a) (srcSpanStart b))

-- | A generalisation of 'groupBy' to functions which aren't equivalences - a group ends
-- when comparison fails with the previous element, rather than the first of the group.
groupBy' :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupBy' eq = flip foldr [] $ \x -> \case
  [] -> [pure x]
  (y :| ys) : zs ->
    if x `eq` y
      then (x :| y : ys) : zs
      else pure x : (y :| ys) : zs

-- | Convert 'Text' to a 'StringBuffer' by making a copy.
textToStringBuffer :: Text -> StringBuffer
textToStringBuffer txt = unsafePerformIO $ do
  buf <- mallocPlainForeignPtrBytes (len + 3)
  withForeignPtr buf $ \ptr -> do
    TFFI.unsafeCopyToPtr txt ptr
    -- last three bytes have to be zero for easier decoding
    pokeElemOff ptr len 0
    pokeElemOff ptr (len + 1) 0
    pokeElemOff ptr (len + 2) 0
  pure StringBuffer {buf, len, cur = 0}
  where
    len = TFFI.lengthWord8 txt

-- | Convert GHC's 'ModuleName' into the one used by Cabal.
ghcModuleNameToCabal :: GHC.ModuleName -> ModuleName
ghcModuleNameToCabal = ModuleName.fromString . GHC.moduleNameString
