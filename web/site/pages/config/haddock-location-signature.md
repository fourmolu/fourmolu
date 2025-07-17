# `haddock-location-signature`

$info$

When not specified, defaults to:
* If `function-arrows=leading` => `trailing`
* If `function-arrows=leading-args` => `trailing`
* If `function-arrows=trailing` => `leading`

## Examples

```fourmolu-example-input
func ::
    -- | Arg 1
    Int ->
    -- | Arg 2
    String ->
    Int -- ^ Return value
```
```fourmolu-example-tab
leading arrows + leading haddocks
{ "function-arrows": "leading", "haddock-location-signature": "leading" }
```
```fourmolu-example-tab
trailing arrows + leading haddocks
{ "function-arrows": "trailing", "haddock-location-signature": "leading" }
```
```fourmolu-example-tab
leading arrows + trailing haddocks
{ "function-arrows": "leading", "haddock-location-signature": "trailing" }
```
```fourmolu-example-tab
trailing arrows + trailing haddocks
{ "function-arrows": "trailing", "haddock-location-signature": "trailing" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/haddock-location-signature).

## See also

* [`function-arrows`](/config/function-arrows)
