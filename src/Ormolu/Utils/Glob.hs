module Ormolu.Utils.Glob
    ( Glob,
      mkGlob,
      matchesGlob,
    )
where

import Data.List (elemIndex, stripPrefix)
import Data.Maybe (fromMaybe)

data Glob
    = MatchEOI
    | MatchExactly !String !Glob
    | SingleWildcard !Glob
    | DoubleWildcard !Glob

mkGlob :: String -> Glob
mkGlob s = case s of
    [] ->
        MatchEOI
    '*' : '*' : t ->
        DoubleWildcard (mkGlob t)
    '*' : t ->
        SingleWildcard (mkGlob t)
    t ->
        let (m, t') = break (== '*') t
         in MatchExactly m (mkGlob t')

matchesGlob :: String -> Glob -> Bool
matchesGlob s g = case g of
    MatchEOI ->
        null s
    MatchExactly p g' ->
        case stripPrefix p s of
            Nothing -> False
            Just s' -> matchesGlob s' g'
    SingleWildcard g' ->
        let l = fromMaybe (length s) (elemIndex '.' s)
         in or [matchesGlob s' g' | i <- [0 .. l], let s' = drop i s]
    DoubleWildcard g' ->
        or [matchesGlob s' g' | i <- [0 .. length s], let s' = drop i s]