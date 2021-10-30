data PlusLevel' t
    = Plus
    -- ^ @n + ℓ@.
        Integer
        (LevelAtom' t)
    deriving (Show, Data)
