module Main where

newtype A = A Int
  deriving newtype (Num)
  deriving stock (Show, Eq)
  deriving stock (Ord, Generic)
  deriving (ToJSON)
