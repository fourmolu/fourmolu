# `import-export-style`

$info$

## Examples

```fourmolu-example-input
module MyModule (
    User (..),
    createUser,
    deleteUser,
) where

import MyModule.Internal (
    User,
    createUser,
    deleteUser,
 )
```
```fourmolu-example-tab
diff-friendly
{ "import-export-style": "diff-friendly" }
```
```fourmolu-example-tab
leading
{ "import-export-style": "leading" }
```
```fourmolu-example-tab
trailing
{ "import-export-style": "trailing" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/import-export).

## See also

* [`comma-style`](/config/comma-style)
