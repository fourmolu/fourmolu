module Ormolu.Utils.Glob
    ( Glob,
      mkGlob,
      matchesGlob,
    )
where

import Control.Monad (msum)
import Data.List (stripPrefix, tails)

data Glob
    = MatchesAnything
    | MatchesExactly !String
    | MatchesPrefix !String !Glob
    | MatchesUntil !String !Glob
    deriving (Eq, Show)

isWildcard :: Char -> Bool
isWildcard = (== '*')

mkGlob :: String -> Glob
mkGlob = globFor
    where
        globFor s =
            case break isWildcard s of
                (s', "") -> MatchesExactly s'
                ("", t) -> globUntil t
                (p, t) -> MatchesPrefix p (globUntil t)
        globUntil s = case dropWhile isWildcard s of
            "" ->
                MatchesAnything
            s' ->
                let (p, t) = break isWildcard s'
                 in MatchesUntil p (globFor t)

matchesGlob :: String -> Glob -> Bool
matchesGlob s g = case g of
    MatchesAnything ->
        True
    MatchesExactly s' ->
        s == s'
    MatchesPrefix p g' ->
        case stripPrefix p s of
            Nothing -> False
            Just t -> t `matchesGlob` g'
    MatchesUntil i g' ->
        case msum (stripPrefix i <$> tails s) of
            Nothing -> False
            Just r -> r `matchesGlob` g'