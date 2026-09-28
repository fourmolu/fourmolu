main = do
    rows
        :: [Row]
        <- query connection limit
    count
        :: Int <-
        countRows rows
    total :: Int <- sumRows rows
    print total
