# `single-constraint-parens`

$info$

The `auto` option will keep parentheses if they already exist, but won't add parentheses if not.

## Examples

```fourmolu-example-input
logAction :: MonadLogger m => String -> m ()
logAction msg = logDebugN $$ "Action: " <> msg
```
```fourmolu-example-tab
With parens
{ "single-constraint-parens": "always" }
```
```fourmolu-example-tab
Without parens
{ "single-constraint-parens": "never" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/single-constraint-parens).
