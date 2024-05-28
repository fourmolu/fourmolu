oneLineParams ::
    a -> b -> c -> d -> (a, b, c, d)
multiLineParams ::
    a ->
    b ->
    c ->
    d ->
    (a, b, c, d)
forallOneLineParams ::
    forall a b c d. a -> b -> c -> d -> (a, b, c, d)
forallNewlineOneLineParams ::
    forall a b c d.
    a -> b -> c -> d -> (a, b, c, d)
forallNewlineMultiLineParams ::
    forall a b c d.
    a ->
    b ->
    c ->
    d ->
    (a, b, c, d)
constraintsNewlineOneLineParams ::
    (C1, C2, C3) =>
    a -> b -> c -> d -> (a, b, c, d)
constraintsNewlineMultiLineParams ::
    (C1, C2, C3) =>
    a ->
    b ->
    c ->
    d ->
    (a, b, c, d)
-- Edge cases with several =>

manyConstraintsNewlineOneLineParams ::
    (Show a) =>
    (Eq a) =>
    (Num a) =>
    a -> a -> ()
manyConstraintsNewlineMultiLineParams ::
    (Show a) =>
    (Eq a) =>
    (Num a) =>
    a ->
    a ->
    ()
manyConstraintsNewlineMultiLineParams ::
    (Show a) =>
    (Eq a) =>
    (Num a) =>
    a ->
    a ->
    ()
-- The first occurence of the one-line parameters won't be preserved because
-- they are a part of a larger multi-line span that goes till the end of the signature.
constraintsOneLineParamsConstraintsOneLineParams ::
    (Show a) =>
    a ->
    a ->
    a ->
    (Eq a) =>
    (Num a) =>
    a -> a -> ()
constraintsMultiLineParamsConstraintsOneLineParams ::
    (Show a) =>
    a ->
    a ->
    a ->
    (Eq a) =>
    (Num a) =>
    a -> a -> ()
constraintsOneLineParamsConstraintsMultiLineParams ::
    (Show a) =>
    a ->
    a ->
    a ->
    (Eq a) =>
    (Num a) =>
    a ->
    a ->
    ()
