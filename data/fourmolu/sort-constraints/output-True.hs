module Main where

f1 :: (Eq a, Show a) => a
f1 a = a

f2 ::
    ( Eq a
    , Show a
    ) =>
    a
f2 a = a

-- We don't currently realise that the inner tuple type
-- is a constraint tuple.
f3 ::
    ( (Show a, Num a)
    , Eq a
    , Generic a
    ) =>
    a
f3 a = a

f4 ::
    (Num a, Show a) =>
    (Eq a) =>
    a
f4 a = a

f5 ::
    ( -- an interloping comment
      Eq a
    , Show a
    ) =>
    a
f5 a = a

class (Eq a, Show a) => Class1 a

data ((Show a, Eq a), Generic a) => A a

deriving instance (Eq a, Show a) => Class1 Int

-- We can't know this is a constraint tuple type rather than a normal
-- tuple type without type information
type MyConstraints a = (Show a, Eq a)

-- https://github.com/fourmolu/fourmolu/issues/451
data Foo = forall a. (A a, B) => Foo a
