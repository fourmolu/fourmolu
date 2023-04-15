# `unicode`

$info$

The `detect` option will check to see if the `-XUnicode` extension is enabled for a file; if so, turns on unicode, otherwise, turns it off.

## Examples

```fourmolu-example-input
{-# LANGUAGE UnicodeSyntax #-}

loadUser :: forall m. MonadIO m => String -> m User
loadUser username = do
  mUser <- getUser username
  maybe (error "Unknown user") pure mUser
```
```fourmolu-example-tab
Without unicode
{ "unicode": "never" }
```
```fourmolu-example-tab
With unicode
{ "unicode": "always" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/unicode-syntax).
