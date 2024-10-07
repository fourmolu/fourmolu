module Main where

-- | Something.
newtype Foo = Foo Int
    deriving stock (Eq, Show, Generic)
    deriving newtype (Num)
    deriving anyclass
        ( ToJSON
        , FromJSON
        )
    deriving (Monoid) via (Sum Int)
    deriving
        (Semigroup)
        via (Sum Int)
