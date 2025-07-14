module Main where

data Foo = Foo
    deriving stock (Show)

data Bar = Bar
    deriving stock (Show, Eq)

data Bat = Bat
    deriving stock (Show)
