# `haddock-style`

$info$

## Examples

```fourmolu-example-input
-- | Get the user with the given username
getUser :: String -> IO (Maybe User)

{- | Same as 'getUsername', except throw
an error if the user doesn't exist.
-}
loadUser :: String -> IO User
```
```fourmolu-example-tab
multi-line
{ "haddock-style": "multi-line" }
```
```fourmolu-example-tab
single-line
{ "haddock-style": "single-line" }
```
```fourmolu-example-tab
multi-line-compact
{ "haddock-style": "multi-line-compact" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/haddock-style).

## See also

* [`haddock-style-module`](/config/haddock-style-module)
