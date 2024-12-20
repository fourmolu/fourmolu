# `sort-constraints`

$info$

This option determines whether constraints should be sorted alphabetically or left alone.

The sorting applies wherever we can determine that a constraint tuple is in fact a constraint tuple type (and not a normal tuple type).
In some situations we cannot determine this and so the constraints are not sorted.

Note that using this option may result in comments inside constraints being moved to unexpected locations.

## Examples

```fourmolu-example-input
f :: (Show a, Eq a) => a
```
```fourmolu-example-tab
Without sorting
{ "sort-constraints": false }
```
```fourmolu-example-tab
With sorting
{ "sort-constraints": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/sort-constraints).
