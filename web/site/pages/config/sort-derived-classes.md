# `sort-derived-classes`

$info$

This option determines whether classes in deriving clauses should be sorted alphabetically or left alone.

Note that using this option may cause comments inside deriving clauses to be misplaced.

## Examples

```fourmolu-example-input
data A
  deriving stock (Show, Eq, Ord, Generic)
```
```fourmolu-example-tab
Without sorting
{ "sort-derived-classes": false }
```
```fourmolu-example-tab
With sorting
{ "sort-derived-classes": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/sort-derived-classes).
