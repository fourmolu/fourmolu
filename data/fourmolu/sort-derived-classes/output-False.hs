module Main where

data A
    deriving (Show, Eq, Ord)

newtype B = B Int
    deriving newtype (Num)
    deriving stock (Show, Eq, Generic)
    deriving (Semigroup, Monoid) via (Generically B)
    deriving stock (Ord, Generic)
    deriving (ToJSON)

data C
    deriving
        ( Show
        , Eq
        , -- A comment that will end up in an odd place
          Ord
        )
