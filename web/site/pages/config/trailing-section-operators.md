# `trailing-section-operators`

$info$

This option determines if "section" operators (those that are `infixr 0`, such as `$`) are left in a trailing position or not.

## Examples

```fourmolu-example-input
foo :: IO ()
foo =
  $ bar
  $ baz
  $ bat
  $ quiz x
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
