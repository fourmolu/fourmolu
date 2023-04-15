# `comma-style`

$info$

## Examples

```fourmolu-example-input
data User = User
    { userName :: String
    , userAge :: Int
    , userActive :: Bool
    }

forbiddenPasswords :: [String]
forbiddenPasswords =
    [ "password"
    , "hunter2"
    , "123456"
    ]
```
```fourmolu-example-tab
leading
{ "comma-style": "leading" }
```
```fourmolu-example-tab
trailing
{ "comma-style": "trailing" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/comma-style).

## See also

* [`import-export-style`](/config/import-export-style)

