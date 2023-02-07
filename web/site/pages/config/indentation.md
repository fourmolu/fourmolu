# `indentation`

$info$

## Examples

```fourmolu-example-input
login :: IO ()
login = do
    name <- getLine
    if name == "admin"
        then putStrLn "Logged in!"
        else do
            putStrLn "Incorrect username"
            login
```
```fourmolu-example-tab
4 spaces
{ "indentation": 4 }
```
```fourmolu-example-tab
2 spaces
{ "indentation": 2 }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/indentation).

## See also

* [`indent-wheres`](/config/indent-wheres)
