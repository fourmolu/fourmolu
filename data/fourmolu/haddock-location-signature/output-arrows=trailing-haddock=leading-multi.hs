functionSig ::
    -- | first argument
    Int ->
    {- | second argument
    with multiline doc
    -}
    Int ->
    -- | result
    String
functionSigWithForall ::
    forall a.
    -- | first argument
    Int ->
    {- | second argument
    with multiline doc
    -}
    a ->
    -- | result
    String
functionSigWithContext ::
    (HasCallStack) =>
    -- | first argument
    Int ->
    {- | second argument
    with multiline doc
    -}
    Int ->
    -- | result
    String
functionSigWithForallAndContext ::
    forall a.
    (HasCallStack) =>
    -- | first argument
    Int ->
    {- | second argument
    with multiline doc
    -}
    a ->
    -- | result
    String
