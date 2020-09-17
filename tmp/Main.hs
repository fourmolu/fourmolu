{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE

main :: IO ()
main = do
    forever $ do
        threadDelay 500_000

data NewUDevice = NewUDevice {name :: ByteString, uniq :: Maybe (), keys :: [Int], absAxes :: [(Int, NewUDevice)]}

-- only matches keys and axes (note DS4 also has FF events)
ps4opts :: ByteString -> NewUDevice
ps4opts name =
    NewUDevice
        { name = name
        , uniq = Nothing
        , keys =
              [ 0
              , 1
              , 12
              ]
        , absAxes =
              zip
                  [ 0
                  , 1
                  , 7
                  ]
                  $ repeat $
                      NewUDevice
                          { name = ""
                          , uniq = Just ()
                          , keys = []
                          , absAxes = []
                          }
        }

xx :: (Num a1, Num a2, Num b, Num c) => ((a1, [a2]), b, c)
xx =
    ( ( 1
      , [ 2
        , 2
        , 2
        ]
      )
    , 3
    , (4)
    )

t :: Num a => [[[a]]]
t =
    [ [[1, 2, 3]]
    , [ []
      , []
      , []
      ]
    ]

t' :: Num a => [[[a]]]
t' =
    [ [[1, 2, 3]]
    ,
        [ []
        , []
        , []
        ]
    ]
-- >>> min1 $ 1 :| [3,7,4]
-- 1
min1 :: Ord a => NonEmpty a -> a
min1 (x :| xs) = case NE.nonEmpty xs of
    Just xs' ->
        let m = min1 xs'
         in if x < m then x else m
    Nothing -> x

min1' :: Ord a => NonEmpty a -> a
min1' (x :| xs) = case min2 xs of
    Just m -> if x < m then x else m
    Nothing -> x

-- >>> min2 [1,3,7,4]
-- Just 1
min2 :: Ord a => [a] -> Maybe a
min2 = \case
    [] -> Nothing
    x : xs ->
        Just $ case min2 xs of
            Nothing -> x
            Just m' -> if x < m' then x else m'

min2' :: Ord a => [a] -> Maybe a
min2' = fmap min1 . NE.nonEmpty

min2'' :: Ord a => [a] -> Maybe a
min2'' = \case
    [] -> Nothing
    x : xs -> Just $ min1 $ x :| xs

head1 :: NonEmpty a -> a
head1 (x :| xs) = maybe x id $ head2 xs

head2 :: [a] -> Maybe a
head2 = fmap NE.head . NE.nonEmpty

f1 :: (NonEmpty a -> b) -> [a] -> Maybe b
f1 f = fmap f . Just . NE.fromList

-- f2 :: ([a] -> Maybe b) -> NonEmpty a -> b
-- f2 g xs = _

-- f2' :: ([a] -> Maybe a) -> NonEmpty a -> a
-- f2' g (x :| xs) = maybe x id $ g xs
