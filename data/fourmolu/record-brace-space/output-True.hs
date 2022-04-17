{-# LANGUAGE RecordWildCards #-}

data Foo = Foo
    { a :: Int
    , b :: Int
    , c :: Int
    }

x :: Foo -> Int
x Foo {a = 42} = a

y :: Foo -> Int
y Foo {a, b, c} = a + b + c

z :: Foo -> Int
z Foo {..} = a + b + c
