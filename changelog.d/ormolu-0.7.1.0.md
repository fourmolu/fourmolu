* Include `base` fixity information when formatting a Haskell file that's
  not mentioned in an existing cabal file. [Issue
  1032](https://github.com/tweag/ormolu/issues/1032)

* Update `displayException` for `OrmoluException` to pretty print the
  exception. [PR 1031](https://github.com/tweag/ormolu/pull/1031).

* Ormolu is now aware of more common module re-exports by default.

* Support explicit mention of target package name in module re-exports. Even
  if the exported package is not specified as a direct dependency of the
  component being formatted it will still be taken into account correctly.
  [Issue 1037](https://github.com/tweag/ormolu/issues/1037).

* Ormolu no longer fails when CPP directly follows the import section (a
  regression introduced in 0.7.0.0). [Issue
  1040](https://github.com/tweag/ormolu/issues/1040).
