module Main where

f1 :: (Show a, Eq a) => a
f1 a = a

f2 ::
    ( Show a
    , Eq a
    ) =>
    a
f2 a = a

-- We don't currently realise that the inner tuple type
-- is a constraint tuple.
f3 ::
    ( Generic a
    , (Show a, Num a)
    , Eq a
    ) =>
    a
f3 a = a

f4 ::
    (Show a, Num a) =>
    (Eq a) =>
    a
f4 a = a

f5 ::
    ( Show a
    , -- an interloping comment
      Eq a
    ) =>
    a
f5 a = a

class (Show a, Eq a) => Class1 a

data (Generic a, (Show a, Eq a)) => A a

deriving instance (Show a, Eq a) => Class1 Int

-- We can't know this is a constraint tuple type rather than a normal
-- tuple type without type information
type MyConstraints a = (Show a, Eq a)

-- https://github.com/fourmolu/fourmolu/issues/451
data Foo = forall a. (B, A a) => Foo a
