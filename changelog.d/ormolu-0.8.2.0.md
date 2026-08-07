* Overhaul how operator fixity information is collected. In addition to the
  Hoogle database, Ormolu now parses the sources of a curated set of important
  packages directly, which yields more accurate and complete fixity data than
  before. In particular it recovers fixities for operators re-exported through
  umbrella modules (e.g. `Servant.API`, `Control.Lens`) that recent Hoogle
  databases no longer record. As a result, formatting of operator chains is
  improved out of the box, and users should expect some operator-heavy code to
  be laid out differently (and more correctly) than in previous releases.

* Improve the layout of chains of `infixr 0` operators (`$`, `seq`, `?:`, and
  the like). Such operators are only laid out in the trailing "staircase" style
  when it is warranted: either the chain consists of a single operator, or its
  final operand is a hanging construct (a `do` block, lambda, `case`, etc.). A
  chain of several such operators that ends in an ordinary expression is now
  laid out with the operators in the leading position instead of an
  ever-deepening pyramid. [Issue
  1151](https://github.com/tweag/ormolu/issues/1151).

* Do not crash when a parent directory cannot be read due to insufficient
  permissions while searching for configuration files; the search for
  configuration files is stopped at that point instead. [Issue
  1212](https://github.com/tweag/ormolu/issues/1212).

* Preserve blank lines between blocks in layout contexts (`where`, `do`,
  `let`) when the preceding block ends with a trailing comment. [Issue
  1132](https://github.com/tweag/ormolu/issues/1132).

* Fix printing of single line export lists with inlined Haddock comments.
  [Issue 1051](https://github.com/tweag/ormolu/issues/1051).

* Fix preservation of the position of comments around the `where` keyword.
  [Issue 784](https://github.com/tweag/ormolu/issues/784).

* Do not sort `Prelude` to the end of the import list when the
  `NoImplicitPrelude` extension is enabled; instead sort it like any other
  import. [Issue 1189](https://github.com/tweag/ormolu/issues/1189).
