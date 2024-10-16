module Main where

newtype A = A Int
    deriving newtype (Num)
    deriving stock (Show, Eq)
    deriving stock (Ord, Generic)
    deriving (ToJSON)

data B
    -- A comment that will end up in an odd place
    deriving stock (Show)
    deriving (Eq)
