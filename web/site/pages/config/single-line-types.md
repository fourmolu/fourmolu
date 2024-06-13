# `single-line-types`

$info$

The `auto` option keeps parameters in a single line, ignoring whether foralls/constraints are on a different line. This allows type to be treated separately from foralls/constraints, and use newlines before type without forcing the whole signature to be formatted multi-line.

## Examples

```fourmolu-example-input
mapM :: Monad m =>
  (a -> m b) -> t a -> m (t b)
```
```fourmolu-example-tab
auto
{ "single-line-types": "auto" }
```
```fourmolu-example-tab
only-if-already-single
{ "single-line-types": "only-if-already-single" }
```

## See also

* [`respectful`](/config/respectful)
