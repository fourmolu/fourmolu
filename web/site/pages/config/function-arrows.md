# `function-arrows`

$info$

## Examples

```fourmolu-example-input
show2WithLabel ::
    forall a b.
    (Show a, Show b) =>
    String ->
    (a, b) ->
    String
```
```fourmolu-example-tab
trailing
{ "function-arrows": "trailing" }
```
```fourmolu-example-tab
leading
{ "function-arrows": "leading" }
```
```fourmolu-example-tab
leading-args
{ "function-arrows": "leading-args" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/function-arrows).
