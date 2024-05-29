* Switched to `ghc-lib-parser-9.10`, with the following new syntactic features/behaviors:
  * GHC proposal [#575](https://github.com/ghc-proposals/ghc-proposals/blob/10290a668608d608c3f6c6010be265cf7a02e1fc/proposals/0575-deprecated-instances.rst): deprecated instances.
  * GHC proposal [#281](https://github.com/ghc-proposals/ghc-proposals/blob/10290a668608d608c3f6c6010be265cf7a02e1fc/proposals/0281-visible-forall.rst): visible forall in types of terms.
    Enabled by `RequiredTypeArguments` (enabled by default).
  * `LinearTypes`: `let` and `where` bindings can now be linear, in particular have multiplicity annotations.
  * Using `forall` as an identifier is now a parse error.
  * GHC proposal [#65](https://github.com/ghc-proposals/ghc-proposals/blob/10290a668608d608c3f6c6010be265cf7a02e1fc/proposals/0065-type-infix.rst): namespacing fixity declarations for type names and WARNING/DEPRECATED pragmas.
  * `TypeAbstractions` now supports `@`-binders in lambdas and function equations.
  * Support for the `GHC2024` language.

* Updated to `Cabal-syntax-3.12`.
