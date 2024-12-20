# `sort-deriving-clauses`

$info$

This option determines whether deriving clauses should be sorted alphabetically or left alone.

Note that using this option may cause comments between deriving clauses to be misplaced.

## Examples

```fourmolu-example-input
newtype A = A Int
  deriving newtype (Num)
  deriving (ToJSON)
  deriving stock (Show, Eq, Ord, Generic)
```
```fourmolu-example-tab
Without sorting
{ "sort-deriving-clauses": false }
```
```fourmolu-example-tab
With sorting
{ "sort-deriving-clauses": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/sort-deriving-clauses).
