module Main where

functionName :: (C a) => a
functionName1 :: C a => a
functionName2 :: (C a, D a) => a

-- https://github.com/fourmolu/fourmolu/issues/340
quantifiedConstraint :: (forall a. Show a => Show (f a)) => f Int

-- https://github.com/fourmolu/fourmolu/issues/374
nestedConstraint :: ((c a, c b) => c (f a b)) => Is c a -> Is c b -> Is c (f a b)

-- https://github.com/fourmolu/fourmolu/issues/446
implicitParam :: (?a :: Int) => Int
