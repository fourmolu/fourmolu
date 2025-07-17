functionSig
    :: Int
    -- ^ first argument
    -> Int
    {- ^ second argument
    with multiline doc
    -}
    -> String
    -- ^ result
functionSigWithForall
    :: forall a
     . Int
    -- ^ first argument
    -> a
    {- ^ second argument
    with multiline doc
    -}
    -> String
    -- ^ result
functionSigWithContext
    :: (HasCallStack)
    => Int
    -- ^ first argument
    -> Int
    {- ^ second argument
    with multiline doc
    -}
    -> String
    -- ^ result
functionSigWithForallAndContext
    :: forall a
     . (HasCallStack)
    => Int
    -- ^ first argument
    -> a
    {- ^ second argument
    with multiline doc
    -}
    -> String
    -- ^ result
