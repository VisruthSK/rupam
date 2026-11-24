# Future

Accepted designs that are deliberately not built yet. Each one is specified
far enough that the current design leaves room for it.

## Models and graphics (deferred)

`~` already parses as a formula value and is rejected by the checker until a
library gives it meaning. Model and graphics libraries elaborate formulas and
aesthetics against a table's schema, the same way verbs resolve bare column
names, so `lm(y ~ x + (1 | site), data = visits)` and
`aes(x = revenue, colour = region)` need no new grammar. Each library adds
typed nodes to the core, lowerings for each dialect and preview handlers.

## Typed foreign imports (deferred)

```text
import r "stats" {
  quantile(x: [Float64], probs: Float64): Float64?
}
```

The checker trusts the declaration; generated code checks the returned type at
the boundary. Allowed only when `strict = false`, recorded in the manifest,
and limited to the dialects whose language matches.

## Exporting rupam functions (deferred)

`export let clean_orders(orders) = ...` and `rupam package r` would produce an
R package (and a Python module) that callers use without rupam. Its entry
points check the incoming data frame's columns, since those callers are not
type-checked, and its documentation comes from the types.

## Modules (deferred)

`import helpers` loads `helpers.rp` from the project as a module.

## Decimal numbers (deferred)

A `Decimal` type for money and other exact decimal quantities, so that
`round(0.15, 1)` is `0.2`.

## More dialects (deferred)

R with data.table, R with polars, and Julia with TidierData, each as a dialect
module with its own lowering and capability table.

## Environment setup (deferred)

`rupam env` to create the uv or renv files a project uses. rupam already uses
them when they exist.

## Recursion (deferred)

Recursive functions, compiled as ordinary target functions. They are the only
construct that cannot be lowered to vectorised code, so they will need their
own rule.
