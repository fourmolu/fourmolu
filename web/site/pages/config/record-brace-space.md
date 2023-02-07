# `record-brace-space`

$info$

## Examples

```fourmolu-example-input
getUsername :: User -> String
getUsername User{..} = username
```
```fourmolu-example-tab
Without spacing
{ "record-brace-space": false }
```
```fourmolu-example-tab
With spacing
{ "record-brace-space": true }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/record-brace-space).
