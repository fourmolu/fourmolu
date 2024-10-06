module Main where

data A
    deriving (Eq, Ord, Show)

newtype B = B Int
    deriving (ToJSON)
    deriving stock (Eq, Show)
    deriving stock (Generic, Ord)
    deriving newtype (Num)
