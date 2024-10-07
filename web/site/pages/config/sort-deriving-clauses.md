# `sort-deriving-clauses`

$info$

This option determines whether deriving clauses.

Note that using this option may cause comments between deriving clauses to be misplaced, so disable it if you are finding that it is misplacing comments.

## Examples

```fourmolu-example-input
newtype A = A Int
  deriving newtype (Num)
  deriving (ToJSON)
  deriving stock (Show, Eq, Ord, Generic)
```
```fourmolu-example-tab
With sorting
{ "sort-deriving-clauses": "True" }
```
```fourmolu-example-tab
Without sorting
{ "sort-deriving-clauses": "False" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/sort-deriving-clauses).
