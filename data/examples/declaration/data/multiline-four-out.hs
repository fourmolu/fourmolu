module Main where

-- | Here we have 'Foo'.
data Foo
    = Foo
    -- ^ One
    | Bar
    -- ^ Two
        Int
    | Baz
    -- ^ Three
    deriving
        (Eq, Show)
