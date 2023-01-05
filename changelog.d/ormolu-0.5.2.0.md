* Eliminated the `fixity-th` Cabal flag because it caused issues on GHC 9.4 as
  well as on aarch64. See [issue
  941](https://github.com/tweag/ormolu/issues/941) and [issue
  927](https://github.com/tweag/ormolu/issues/927).

* Now operators without explicitly specified fixity default to left
  associativity and highest precedence. [Issue
  907](https://github.com/tweag/ormolu/issues/907).
