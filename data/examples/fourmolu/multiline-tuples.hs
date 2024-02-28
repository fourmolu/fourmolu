{-# LANGUAGE PatternSynonyms #-}

multilineFunction
    ( a
        , b
            , c
                ) = a + b + c

multilineLambda =
    \( a
        , b
            , c
                ) -> a + b + c

multilineCase = \case
    ( a
        , b
            , c
                ) -> a + b + c

multilineDo = do
    ( a
        , b
            , c
                ) <- _
    pure $ a + b + c

multilineQuasi =
    [p|
        ( a
        , b
        , c
        )
    |]

-- top level
( a
    , b
        , c
            ) = (1, 2, 3)

pattern MultilineSynonym =
    ( 1
        , 2
            , 3
                )
