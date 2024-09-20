module Foo (
    foo,

    -- * Something
    bar,
    {- | A multiline
    comment here
    -}
    baz,

    -- * Another thing
    MyClass (
        class1,
        class2
    ),
)
where

import qualified MegaModule as M (
    Either,
    Maybe (Just, Nothing),
    MaybeT (..),
    Monad (
        return,
        (>>),
        (>>=)
    ),
    MonadBaseControl,
    join,
    liftIO,
    void,
    (<<<),
    (>>>),
 )

{- // -}

-- https://github.com/fourmolu/fourmolu/issues/341
module Foo (
    -- | asdf
    singleExport,
) where

{- // -}

-- https://github.com/fourmolu/fourmolu/issues/381
module Foo (
    -- * Re-export of module
    module X,

    -- * Some other thing
    Foo,
) where

{- // -}

-- See data/examples/import/docstrings-after-exports.hs
module Test (
    since1, -- ^ @since 1.0
    since2, -- ^ @since 2.0
    since3, -- ^ @since 3.0
    SinceType (..), -- ^ @since 4.0
    SinceClass (..), -- ^ @since 5.0
    Multi (..),
    {- ^ since 6.0
    multi
    line
    -}
) where
