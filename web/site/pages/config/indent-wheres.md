# `indent-wheres`

$info$

## Examples

```fourmolu-example-input
setPassword :: String -> String -> IO ()
setPassword username password = do
    user <- getUser username >>= maybe userNotFound pure
    hash <- hashPassword password
    saveUser user{userPasswordHash = hash}
  where
    userNotFound = error $$ "User not found: " ++ username
```
```fourmolu-example-tab
Without indented wheres
{ "indent-wheres": false }
```
```fourmolu-example-tab
With indented wheres
{ "indent-wheres": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/indentation).

## See also

* [`indentation`](/config/indentation)
