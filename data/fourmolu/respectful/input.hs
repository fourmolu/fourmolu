import Foo
import Foo.Inner

import Bar

import Baz

type Foo = Int
type Foo2 = Double

type Bar = Int
type Bar2 = Double

main = do
  x :: Int <- foo
  y
    :: Int
    <- foo
  u :: Int
    <- foo
  z ::
    Int <-
    foo
  w :: Int <- do
    foo
  v :: Int <-
    foo bar
  (r :: Int)
    <- foo
  t <-
    foo bar
  s
    <- foo
  p <- foo
  q
    :: Int
    -- comment
    <- foo

bindInComp =
  [ r
  | r :: Int
      <- foo
  ]

bindInCompOneLine = [r | r :: Int <- foo]

bindInCompOneLineBind =
  [ r
  | r :: Int <- foo
  ]

bindInCompNoSig =
  [ r
  | r <-
      foo
  ]

bindInCompNoSigOneLine = [r | r <- foo]

