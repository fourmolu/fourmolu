{-| This is a test multiline
module haddock
-}
module Foo where

-- | This is a singleline function haddock
single1 :: Int

-- | This is a singleline function haddock
single2 :: Int

{-| This is a multiline
function haddock
-}
multi1 :: Int

{-|
This is a multiline
function haddock
-}
multi2 :: Int

{-| This is a multiline haddock
     with indentation
-}
multi_indentation :: Int

{-| This is a haddock

with two consecutive newlines


https://github.com/fourmolu/fourmolu/issues/172
-}
foo :: Int
foo = 42

{-| This is a haddock containing another haddock

> {\-# LANGUAGE ScopedTypeVariables #-\}
-}
haddock_in_haddock :: Int
