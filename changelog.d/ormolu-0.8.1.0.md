* Fixed printing of guards on pattern binds. [Issue
  1178](https://github.com/tweag/ormolu/issues/1178).

* Switched to `ghc-lib-parser-9.14`, with the following new syntactic features:
   * GHC proposal [#493](https://github.com/ghc-proposals/ghc-proposals/blob/e2c683698323cec3e33625369ae2b5f585387c70/proposals/0493-specialise-expressions.rst): expressions in SPECIALISE pragmas
   * Multiline strings in foreign import declarations.
   * `ExplicitNamespaces` supports the `data` namespace specifier in import and export lists, replacing `pattern`.
   * `LinearTypes` adds new syntax to support non-linear record fields.
   * `RequiredTypeArguments` allows visible forall in GADT syntax.

* Updated to `Cabal-syntax-3.16`.

* Correctly format string literals containing the `\^\` escape sequence. [Issue
  1165](https://github.com/tweag/ormolu/issues/1165).

* Correctly preserve consecutive blank lines in multiline strings. [Issue
  1194](https://github.com/tweag/ormolu/issues/1194).

* Fixed printing of multi-line or-patterns inside as-patterns. [Issue
  1183](https://github.com/tweag/ormolu/issues/1183).

* Fixed an issue where or-patterns would be indented twice. [Issue
  1188](https://github.com/tweag/ormolu/issues/1188).

* Add support for `ExplicitLevelImports`. [Issue
  1192](https://github.com/tweag/ormolu/issues/1192).
