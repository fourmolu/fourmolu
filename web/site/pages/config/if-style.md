# `if-style`

$info$

This option determines whether the keywords `then` and `else` should be indented (the default) or hanging off the current indentation. Indenting looks more consistent with `case` statements, while hanging looks more consistent with if-statements in other languages.

## Examples

```fourmolu-example-input
testUser =
    if condition then do
        first
        branch
    else
        if another condition then do
            second
            branch
        else do
            third
            branch
```
```fourmolu-example-tab
Indented
{ "if-style": "indented" }
```
```fourmolu-example-tab
Hanging
{ "if-style": "hanging" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/if-style).
