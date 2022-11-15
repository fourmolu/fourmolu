* Imports are now sorted by package qualifier, if one is present.
  [Issue 905](https://github.com/tweag/ormolu/issues/905).

* Extension packs like `GHC2021` and `Haskell2010` are now bumped to the top of
  the list of language pragmas. [Issue
  922](https://github.com/tweag/ormolu/issues/922).

* Fix formatting of `SCC` pragmas in `do` blocks. [Issue
  925](https://github.com/tweag/ormolu/issues/925).

* Support type applications in patterns. [Issue
  930](https://github.com/tweag/ormolu/issues/930).

* Handle `UnicodeSyntax` variants more consistently. [Issue
  934](https://github.com/tweag/ormolu/issues/934).

* Fix an inconsistency in formatting of types in GADT declarations in
  certain cases. [PR 932](https://github.com/tweag/ormolu/pull/932).

* Switched to `ghc-lib-parser-9.4`, which brings support for the following new
  syntactic features:
  * `\cases` via `LambdaCase`
  * `OPAQUE` pragmas
  * Unboxed sum type constructors like `(# | #)`.

* Updated to `Cabal-syntax-3.8`, supporting `cabal-version: 3.8`.
