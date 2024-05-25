functionName ::
    (C1, C2, C3) =>
    a -> b -> c -> d -> (a, b, c, d)
foo ::
    (Show a, Eq a) =>
    a -> a -> ()
bar ::
    (Show a) =>
    (Eq a) =>
    (Num a) =>
    a -> a -> ()
contextBetweenParameters ::
    (Show a) =>
    a ->
    a ->
    (Eq a) =>
    (Num a) =>
    a -> a -> ()
