# `column-limit`

$info$

If set to a number, fourmolu will try to break up lines longer than that many characters. This may break idempotence.

## Examples

```fourmolu-example-input
rootUsers :: [String]
rootUsers = ["root"]

adminUsers :: [String]
adminUsers = ["hunter2", "elon.musk", "simon.peyton.jones"]

regularUsers :: [String]
regularUsers = ["alice", "bob", "charlie", "david", "eve", "felicia", "greg", "hailey", "isaac"]
```
```fourmolu-example-tab
No limit
{ "column-limit": "none" }
```
```fourmolu-example-tab
40 characters
{ "column-limit": 40 }
```
```fourmolu-example-tab
80 characters
{ "column-limit": 80 }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/column-limit).
