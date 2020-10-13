f = id $ id $ id $ id $ id $ R{a = 1, b = 2}

f =
    id $
        id $
            id $
                id $
                    id $
                        R
                            { a = 1
                            , b = 2
                            }

f =
    id . id . id . id . id $
        R
            { a = 1
            , b = 2
            }

g =
    a b c d
            { a = 1
            , b = 2
            }
