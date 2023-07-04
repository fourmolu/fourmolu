module Main where

functionName :: (C a) => a
functionName1 :: C a => a
functionName2 :: (C a, D a) => a
-- https://github.com/fourmolu/fourmolu/issues/340
quantifiedConstraint :: (forall a. Show a => Show (f a)) => f Int
