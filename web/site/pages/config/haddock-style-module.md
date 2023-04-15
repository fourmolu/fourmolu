# `haddock-style-module`

$info$

This option determines what Haddock style to use for the module docstring. When this option is set to `null` (the default), uses the same value as `haddock-style`.

## Examples

```fourmolu-example-input
{- |
Module: MyModule
Author: Joe Haskell

The primary API for my library.
-}
module MyModule where
```
```fourmolu-example-tab
multi-line
{ "haddock-style-module": "multi-line" }
```
```fourmolu-example-tab
single-line
{ "haddock-style-module": "single-line" }
```
```fourmolu-example-tab
multi-line-compact
{ "haddock-style-module": "multi-line-compact" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/haddock-style).

## See also

* [`haddock-style`](/config/haddock-style)
