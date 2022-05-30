module Foo (
    asdf,
    bar,
) where

data Test
    = Test1
        { a :: Int
        , b :: Int
        }
    | Test2 Bool

x :: IO Int
x = do
    putStrLn "asdf"
    let result =
            y + 10
    return result
  where
    y = 1

class Foo a where
    foo :: a -> Int
