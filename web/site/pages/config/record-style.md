# `record-style`

$info$

## Examples

```fourmolu-example-input
data User = User
    { userName :: String
    , userAge :: Int
    , userActive :: Bool
    }

defaultUser = User
    { userName = ""
    , userAge = -1
    , userActive = false
    }
```
```fourmolu-example-tab
aligned
{ "record-style": "aligned", "comma-style": "trailing" }
```
```fourmolu-example-tab
knr
{ "record-style": "knr", "comma-style": "trailing" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/record-style).

## See also

* [`import-export-style`](/config/import-export-style)
