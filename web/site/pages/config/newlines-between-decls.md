# `newlines-between-decls`

$info$

## Examples

```fourmolu-example-input
newtype Username = Username String

newtype Password = Password String
```
```fourmolu-example-tab
With 1 newline
{ "newlines-between-decls": 1 }
```
```fourmolu-example-tab
With 2 newlines
{ "newlines-between-decls": 2 }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/newlines-between-decls).

## See also

* [`respectful`](/config/respectful)
