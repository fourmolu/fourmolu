# `let-style`

$info$

This option determines whether the clauses in a `let` block should be on the same line as the `let`/`in` keywords (`inline`) or on the next line (`newline`).

The `auto` style (the default) uses whatever style the user uses at the call-site. This does mean that a codebase might have different `let` styles, but it allows the user to decide which style makes the most sense on a case-by-case basis.

The `mixed` style uses `inline` if the `let` binding has only one binding or `newline` if the `let` binding has multiple.

## Examples

```fourmolu-example-input
testUser =
    let name = "Alice"
        age = 30
    in User name age
```
```fourmolu-example-tab
inline
{ "let-style": "inline" }
```
```fourmolu-example-tab
newline
{ "let-style": "newline" }
```

For more examples, see the [test files](https://github.com/fourmolu/fourmolu/tree/main/data/fourmolu/let-style).

## See also

* [`indentation`](/config/indentation)
* [`in-style`](/config/in-style)
