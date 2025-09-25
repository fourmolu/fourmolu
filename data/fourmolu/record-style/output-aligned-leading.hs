data Foo = Foo
    { a :: Int
    , b :: Int
    , c :: Int
    }

-- Test nesting
data Client
    = PremiumClient
        { name :: String
        , address :: String
        , aFieldWithAnUnneccessarilyLongFieldName :: String
        }
    | -- And some comment here as well
      NotSoPremiumClient
        { name :: String
        , address :: String
        , aFieldWithAnUnneccessarilyLongFieldName :: String
        }

x :: Foo -> [Int]
x
    Foo
        { a = possiblyLongName
        , b = anotherLongName
        , c = longNameAsWell
        } = [possiblyLongName, anotherLongName, longNameAsWell]

y :: Int -> Int -> Int -> Foo
y possiblyLongName anotherLongName longNameAsWell =
    Foo
        { a = possiblyLongName
        , b = anotherLongName
        , c = longNameAsWell
        }
