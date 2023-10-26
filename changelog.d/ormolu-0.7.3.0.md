* Switched to `ghc-lib-parser-9.8`, with the following new syntactic features:
  * `ExtendedLiterals`: `123#Int8` is a literal of type `Int8#`. (disabled by
    default)
  * `TypeAbstractions`: `@k`-binders in data type declarations (enabled by
    default)
  * GHC proposal [#134](https://github.com/ghc-proposals/ghc-proposals/blob/0b652bd70258e354dfe4a05940182007596f8bf7/proposals/0134-deprecating-exports-proposal.rst): deprecating/warning about exports
  * GHC proposal [#541](https://github.com/ghc-proposals/ghc-proposals/blob/0b652bd70258e354dfe4a05940182007596f8bf7/proposals/0541-warning-pragmas-with-categories.rst): warning categories
