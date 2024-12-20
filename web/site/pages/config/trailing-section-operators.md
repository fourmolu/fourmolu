# `trailing-section-operators`

$info$

## Examples

```fourmolu-example-input
foo :: IO ()
foo =
  bar
    $$ baz
    $$ bat
    $$ quiz x
```
```fourmolu-example-tab
With trailing section operators
{ "trailing-section-operators": true }
```
```fourmolu-example-tab
Without trailing section operators
{ "trailing-section-operators": false }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/trailing-section-operators).
