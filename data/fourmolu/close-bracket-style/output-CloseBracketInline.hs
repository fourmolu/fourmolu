x :: [Int]
x =
  [ 1,
    2,
    3 ]

y :: (Int, Bool, Double)
y =
  ( 1,
    True,
    1.5 )

z :: IO ()
z =
  ( \x -> do
      print "hello"
      print x )
    (1, 2, 3)

data Foo = Foo
  { a :: Int,
    b :: Bool,
    c :: Double }
