data ErrorMessage' s
    = Text
    -- ^ Show the text as is.
        s
    | forall t. ShowType
    -- ^ Pretty print the type.
    -- @ShowType :: k -> ErrorMessage@
        t
    | -- | Put two pieces of error message next
      -- to each other.
      ErrorMessage' s :<>: ErrorMessage' s
    | -- | Stack two pieces of error message on top
      -- of each other.
      ErrorMessage' s :$$: ErrorMessage' s
