{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnicodeSyntax #-}

module UnicodeExample where

import Control.Arrow
import Data.Kind
import Language.Haskell.TH (Exp, Quote)
import Prelude hiding (elem)

data T = MkT {foo :: Int}

data Term a where
    Lit :: Int -> Term Int
    Succ :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If :: Term Bool -> Term a -> Term a -> Term a
    Pair :: Term a -> Term b -> Term (a, b)

newtype Swizzle = MkSwizzle (forall a. (Ord a) => [a] -> [a])

data family GMap k :: Type -> Type

data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)

type family F a where
    F Int = Double
    F Bool = Char
    F a = String

type family Id a = r | r -> a

type instance Id Int = Int

type instance Id Bool = Bool

type family Elem c :: Type

type instance Elem [e] = e

data T1 a = MkT1 a

construct :: a %1 -> T1 a
construct x = MkT1 x

deconstruct :: T1 a %1 -> a
deconstruct (MkT1 x) = x

pattern HeadC x <- x : xs
    where
        HeadC x = [x]

pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

class (Monad m, Monad (t m)) => Transform t m where
    lift :: m a -> (t m) a

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance (Eq a) => Eq (Tree a) where
    Leaf a == Leaf b = a == b
    (Branch l1 r1) == (Branch l2 r2) = (l1 == l2) && (r1 == r2)
    _ == _ = False

add1 :: (Quote m) => Int -> m Exp
add1 x = [|x + 1|]

decl :: (Quote m) => m Decl
decl = [d|data Foo|]

monad = do
    putStr "x: "
    l <- getLine
    return (words l)

arrow f g h = proc x -> do
    y <- f -< x + 1
    g -< 2 * y
    let z = x + y
    t <- h -< x * z
    (| f (\y -> returnA -< y) |) ys
    returnA -< t + z

elem :: (Eq a) => a -> [a] -> Bool
x `elem` [] = False
x `elem` (y : ys) = x == y || (x `elem` ys)

h :: (forall a. a -> a) -> (Bool, Char)
h f = (f True, f 'c')
