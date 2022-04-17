module Foo
  ( foo,

    -- * Something
    bar,

    -- | A multiline
    -- comment here
    baz,

    -- * Another thing
    MyClass
      ( class1
      , class2
      ),
  )
where

import qualified MegaModule as M
  ( Either,
    Maybe (Just, Nothing),
    MaybeT (..),
    Monad
      ( return
      , (>>)
      , (>>=)
      ),
    MonadBaseControl,
    join,
    liftIO,
    void,
    (<<<),
    (>>>),
  )
