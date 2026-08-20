module Main where

newtype A = A Int
    deriving (ToJSON)
    deriving stock (Show, Eq)
    deriving stock (Ord, Generic)
    deriving newtype (Num)

data B
    deriving (Eq)
    -- A comment that will stay above Show
    deriving stock (Show)
