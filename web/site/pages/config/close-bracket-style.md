# `close-bracket-style`

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
newline
{ "close-bracket-style": "newline" }
```
```fourmolu-example-tab
inline
{ "close-bracket-style": "inline" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/close-bracket-style).

## See also

* [`comma-style`](/config/comma-style)
