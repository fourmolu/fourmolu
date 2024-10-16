module Main where

data A
    deriving (Eq, Ord, Show)

newtype B = B Int
    deriving newtype (Num)
    deriving stock (Eq, Generic, Show)
    deriving (Monoid, Semigroup) via (Generically B)
    deriving stock (Generic, Ord)
    deriving (ToJSON)

data C
    deriving
        ( Eq
        , -- A comment that will end up in an odd place
          Ord
        , Show
        )
