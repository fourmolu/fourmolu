module Foo (foo, bar, baz, qux) where

{- ORMOLU_DISABLE -}
foo :: Char
foo =  'a'
{- FOURMOLU_ENABLE -}

bar :: Char
bar =  'b'

{- FOURMOLU_DISABLE -}
baz :: Char
baz =  'c'
{- ORMOLU_ENABLE -}

qux :: Char
qux =  'd'
