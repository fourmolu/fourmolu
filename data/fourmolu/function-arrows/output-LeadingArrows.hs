{-# LANGUAGE InstanceSigs #-}

module Main where

-- | Something else.
class Bar a where
    -- | Bar
    bar
        :: String
        -> String
        -> a
    -- Pointless comment
    default bar
        :: ( Read a
           , Semigroup a
           )
        => a
        -> a
        -> a
    -- Even more pointless comment
    bar
        a
        b =
            read a <> read b

-- | Here goes a comment.
data Foo a where
    -- | 'Foo' is wonderful.
    Foo
        :: forall a b
         . (Show a, Eq b) -- foo
        -- bar
        => a
        -> b
        -> Foo 'Int
    -- | But 'Bar' is also not too bad.
    Bar
        :: Int
        -- ^ An Int
        -> Maybe Text
        -- ^ And a Maybe Text
        -> Foo 'Bool
    -- | So is 'Baz'.
    Baz
        :: forall a
         . a
        -> Foo 'String
    (:~>) :: Foo a -> Foo a -> Foo a

instance Eq Int where
    (==) :: Int -> Int -> Bool
    (==) _ _ = False

instance Ord Int where
    compare
        :: Int
        -> Int
        -> Ordering
    compare
        _
        _ =
            GT

functionName
    :: (C1, C2, C3, C4, C5)
    => a
    -> b
    -> ( forall a
          . (C6, C7)
         => LongDataTypeName
         -> a
         -> AnotherLongDataTypeName
         -> b
         -> c
       )
    -> (c -> d)
    -> (a, b, c, d)

data Record = Record
    { recFun :: Int
                -> Int
                -> Bool
    , recOther :: Bool
    }
