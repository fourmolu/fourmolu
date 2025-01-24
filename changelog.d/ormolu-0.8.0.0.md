* Format multiple files in parallel. [Issue
  1128](https://github.com/tweag/ormolu/issues/1128).

* Fractional precedences are now allowed in `.ormolu` files for more precise
  control over formatting of complex operator chains. [Issue
  1106](https://github.com/tweag/ormolu/issues/1106).

* Correctly format type applications of `QuasiQuotes`. [Issue
  1134](https://github.com/tweag/ormolu/issues/1134).

* Correctly format multi-line parentheses in arrow `do` blocks. [Issue
  1144](https://github.com/tweag/ormolu/issues/1144).

* Switched to `ghc-lib-parser-9.12`, with the following new syntactic features:
   * GHC proposal [#522](https://github.com/ghc-proposals/ghc-proposals/blob/c9401f037cb22d1661931b2ec621925101052997/proposals/0522-or-patterns.rst): `OrPatterns` (enabled by default)
   * GHC proposal [#569](https://github.com/ghc-proposals/ghc-proposals/blob/c9401f037cb22d1661931b2ec621925101052997/proposals/0569-multiline-strings.rst): `MultilineStrings` (disabled by default)
   * GHC proposal [#409](https://github.com/ghc-proposals/ghc-proposals/blob/f79438cf8dbfcd90187f7af3a380515ffe45dbdc/proposals/0409-exportable-named-default.rst): `NamedDefaults` (enabled by default)
   * GHC proposal [#281](https://github.com/ghc-proposals/ghc-proposals/blob/c9401f037cb22d1661931b2ec621925101052997/proposals/0281-visible-forall.rst): accept more types in terms: `forall` quantifications, constraint arrows `=>`, type arrows `->` (enabled by default)
   * Part of GHC proposal [#425](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0425-decl-invis-binders.rst): wildcard binders (enabled by default)

* Correctly format non-promoted type-level tuples with `NoListTuplePuns`. [Issue
  1146](https://github.com/tweag/ormolu/issues/1146).

* Updated to `Cabal-syntax-3.14`. [Issue
  1152](https://github.com/tweag/ormolu/issues/1152).
