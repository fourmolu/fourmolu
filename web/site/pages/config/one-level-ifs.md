# `one-level-ifs`

$info$

This option determines whether the keywords `then` and `else` should have their own level of indentantion.

`false` value (default) looks more consistent with `case` branches.

`true` looks more consistent with other languages.

## Examples

```fourmolu-example-input
testUser =
    if condition
        then do
            first
            branch
        else do
            second
            branch
```
```fourmolu-example-tab
Disable one-level
{ "one-level-ifs": false }
```
```fourmolu-example-tab
Enable one-level
{ "one-level-ifs": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/one-level-ifs).
