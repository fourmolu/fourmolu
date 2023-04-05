## Ormolu 0.6.0.1

* Fix false positives in AST diffing related to `UnicodeSyntax`. [PR
  1009](https://github.com/tweag/ormolu/pull/1009).

## Ormolu 0.6.0.0

* Haddocks attached to arguments of a data constructor are now formatted in
  the pipe style (rather than the caret style), consistent with everything
  else. As a consequence, now Ormolu's output will be deemed invalid by the
  Haddock shipped with GHC <9.0. [Issue
  844](https://github.com/tweag/ormolu/issues/844) and [issue
  828](https://github.com/tweag/ormolu/issues/828).

* Insert space before char literals in ticked promoted constructs when
  necessary. [Issue 1000](https://github.com/tweag/ormolu/issues/1000).

* Switched to `ghc-lib-parser-9.6`:
  * Extended `OverloadedLabels`: `#Foo`, `#3`, `#"Hello there"`.

    Also, it is now disabled by default, as it causes e.g. `a#b` to be parsed
    differently.
  * New extension: `TypeData`, enabled by default.
  * Parse errors now include error codes, cf. https://errors.haskell.org.

* Updated to `Cabal-syntax-3.10`.

* Now whenever Ormolu fails to parse a `.cabal` file it also explains why.
  [PR 999](https://github.com/tweag/ormolu/pull/999).
