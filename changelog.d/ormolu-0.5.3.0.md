* Stop making empty `let`s move comments. [Issue
  917](https://github.com/tweag/ormolu/issues/917).

* Now `.ormolu` fixity override files can use both LF and CRLF line endings.
  [PR 969](https://github.com/tweag/ormolu/pull/969).

* Normalize parentheses around constraints. [Issue
  264](https://github.com/tweag/ormolu/issues/264).

* The `ormolu` function now consumes `Text` instead of `String` due to an
  internal refactoring.

* Exposed a more complete public API in the `Ormolu` module. The API is
  supposed to be stable and change according to
  [PVP](https://pvp.haskell.org/).

* Now warnings regarding Ormolu not being able to find `.cabal` files or
  finding such files but them not mentioning the source file in question are
  only displayed when `--debug` is used. Printing the warnings by default
  seems to have been confusing, see e.g. [Issue
  971](https://github.com/tweag/ormolu/issues/971) and [issue
  924](https://github.com/tweag/ormolu/issues/924).
