* Correctly format edge cases where fully collapsing string gaps changes the
  string represented by a string literal. [Issue
  1160](https://github.com/tweag/ormolu/issues/1160).

* Fix false positives in AST diffing in fixity declarations with implicit
  fixity, such as `infix +`. [Issue
  1166](https://github.com/tweag/ormolu/issues/1166).

* Make multiline function signatures in RequiredTypeArguments consistent with
  types [PR 1170](https://github.com/tweag/ormolu/pull/1170)

* Correctly format single-line `MultiWayIf`s. [Issue
  1171](https://github.com/tweag/ormolu/issues/1171).
