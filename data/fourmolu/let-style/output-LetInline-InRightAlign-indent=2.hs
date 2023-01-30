{-- should be the same in every option --}

let_oneline_empty =
  let in 10

let_oneline_single =
  let a = 1 in a + 2

let_oneline_multi =
  let a = 1; b = 2 in a + b

{-- pure let expressions --}

let_empty =
  let
   in 10

let_single =
  let a = 1
   in a + 2

let_single_sig =
  let a :: Int
      a = 1
   in a + 2

let_single_comment =
  let -- a comment
      a = 1
   in a + 2

let_multi =
  let a = 1
      b = 2
   in a + b

let_single_newline =
  let a = 1
   in a + 2

let_multi_newline =
  let a = 1
      b = 2
   in a + b

{-- do-block --}

test_do = do
  let

  let a = 1

  let b = 2
      c = 3

  let d = "hello"
   in print d

  let d = "hello"
      e = "world"
   in print (d ++ e)

  let f = 1 in print f

{-- list comprehension --}

test_list =
  [ x + a + b + c
  | x <- xs
  , let
  , let a = 1
  , let b = 2
        c = 2
  ]

test_list_do = do
  x <-
    [ x + a + b + c
      | x <- xs
      , let
      , let a = 1
      , let b = 2
            c = 3
      ]

  [ x + y + a + b + c
    | y <- ys
    , let
    , let a = 1
    , let b = 2
          c = 3
    ]
