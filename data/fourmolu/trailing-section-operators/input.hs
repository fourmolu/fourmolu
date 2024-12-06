someShortFunction $ someShortFunction x

someShortFunction $ someShortFunction $ \x -> do
  putStrLn x

someLongFunction $ someLongFunction
  $ someLongFunction
  $ someLongFunction $ someLongFunction x

someLongFunction
  $ someLongFunction
  $ someLongFunction $ someLongFunction $ \x -> do
    putStrLn x
