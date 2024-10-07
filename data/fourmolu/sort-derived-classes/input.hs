module Main where

data A
  deriving (Show, Eq, Ord)

newtype B = B Int
  deriving newtype (Num)
  deriving stock (Show, Eq)
  deriving stock (Ord, Generic)
  deriving (ToJSON)
