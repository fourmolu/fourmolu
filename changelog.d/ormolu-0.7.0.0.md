* Inference of operator fixity information is now more precise and takes
  into account the import section of the module being formatted. [Issue
  892](https://github.com/tweag/ormolu/issues/892) and [issue
  929](https://github.com/tweag/ormolu/issues/929).

* Ormolu can now be made aware of module re-exports through either special
  declarations in `.ormolu` files (see the readme for a description of the
  syntax), or on the command line with the `--reexport`/`-r` option. [Issue
  1017](https://github.com/tweag/ormolu/issues/1017).

* Ormolu now looks for `.ormolu` files independently of `.cabal` files. This
  means that it is now possible to have one `.ormolu` file for multiple
  Cabal packages. [Issue 1019](https://github.com/tweag/ormolu/issues/1019).

* Consistently format `do` blocks/`case`s/`MultiWayIf`s with 4 spaces if and
  only if they occur as the applicand. [Issue
  1002](https://github.com/tweag/ormolu/issues/1002) and [issue
  730](https://github.com/tweag/ormolu/issues/730).

* Support the (deprecated) `DatatypeContexts` extension to avoid surprises.
  [Issue 1012](https://github.com/tweag/ormolu/issues/1012).

* Don't let comments escape from empty export lists. [Issue
  906](https://github.com/tweag/ormolu/issues/906).

* Format `\cases` with multiple patterns across multiple lines correctly. [Issue
  1025](https://github.com/tweag/ormolu/issues/1025).
