# `local-modules`

$info$

Modules to consider as part of the current Cabal package, for the [`import-grouping`](/config/import-grouping) option.

This is automatically detected from the Cabal file, but can be useful to set manually when you don't have a Cabal file.

The following is an example of using the import grouping feature with manually set local modules using the CLI:

```shell
fourmolu Main.hs --import-grouping by-scope --local-modules SomeInternal.Module1 --local-modules SomeModule
```
