-- https://github.com/fourmolu/fourmolu/pull/64

module Foo (
    -- | Multiline comment
    -- in header
    foo,
) where

class Foo a where
    -- | Multiline haddock
    -- in class
    multiline :: a
