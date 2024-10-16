module Main where

newtype A = A Int
    deriving (ToJSON)
    deriving stock (Show, Eq)
    deriving stock (Ord, Generic)
    deriving newtype (Num)

data B
    -- A comment that will end up in an odd place

    deriving (Eq)
    deriving stock (Show)
