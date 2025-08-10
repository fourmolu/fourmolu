# `shifted-ifs`

$info$

This option determines whether the keywords `then` and `else` should have their own level of indentantion.

`false` value (default) adds an extra level of indentation for keywords `then` and `else`. It looks more consistent with `case` branches.

`true` adds indentation only for branches. It looks more consistent with if-else blocks in other languages.

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
Disable shifted ifs
{ "shifted-ifs": false }
```
```fourmolu-example-tab
Enable shifted ifs
{ "shifted-ifs": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/shifted-ifs).
