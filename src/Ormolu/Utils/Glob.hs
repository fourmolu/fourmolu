module Ormolu.Utils.Glob
    ( Glob,
      mkGlob,
      matchesGlob,
    )
where

import Data.List (elemIndex, stripPrefix)
import Data.Maybe (fromMaybe)

newtype Glob = Glob [GlobPart]
    deriving (Eq, Show)

data GlobPart
    = MatchExactly !String
    | SingleWildcard
    | DoubleWildcard
    deriving (Eq, Show)

mkGlob :: String -> Glob
mkGlob = Glob . parsePart
    where
        parsePart :: String -> [GlobPart]
        parsePart s = case s of
            [] ->
                []
            '*' : '*' : t ->
                DoubleWildcard : parsePart t
            '*' : t ->
                SingleWildcard : parsePart t
            t ->
                let (m, t') = break (== '*') t
                 in MatchExactly m : parsePart t'

matchesGlob :: String -> Glob -> Bool
matchesGlob s (Glob ps) = s `matchesGlobParts` ps

matchesGlobParts :: String -> [GlobPart] -> Bool
matchesGlobParts s g = case g of
    [] ->
        null s
    MatchExactly p : g' ->
        case stripPrefix p s of
            Nothing -> False
            Just s' -> s' `matchesGlobParts` g'
    SingleWildcard : g' ->
        let l = fromMaybe (length s) (elemIndex '.' s)
         in or [s' `matchesGlobParts` g' | i <- [0 .. l], let s' = drop i s]
    DoubleWildcard : g' ->
        or [s' `matchesGlobParts` g' | i <- [0 .. length s], let s' = drop i s]
