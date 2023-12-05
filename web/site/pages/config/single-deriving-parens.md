# `single-deriving-parens`

$info$

The `auto` option will keep parentheses if they already exist, but won't add parentheses if not.

## Examples

```fourmolu-example-input
data Foo = Foo
  deriving stock Show
```
```fourmolu-example-tab
With parens
{ "single-deriving-parens": "always" }
```
```fourmolu-example-tab
Without parens
{ "single-deriving-parens": "never" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/single-deriving-parens).
