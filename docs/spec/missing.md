# Missing values

A **decision point** is a `filter` predicate, a reduction, a grouping or
`distinct` key, or a join key. An optional value at a decision point requires
a policy:

| Policy | `filter` | reductions | grouping, `distinct` | join keys |
| --- | --- | --- | --- | --- |
| `skip` | drop rows with an unknown predicate | ignore missing values | drop rows with a missing key | missing never matches |
| `propagate` | keep rows with an unknown predicate | result is `None` if any input is | missing is one group | not allowed |
| `error` | fail on an unknown predicate | fail on a missing value | fail on a missing key | fail on a missing key |

## Scope

1. A call: `mean(z, missing = propagate)`.
2. A verb, covering every decision point in it:
   `summarize(m = mean(z), by = site, missing = skip)`.
3. A file: `use missing = skip`.

The most specific applies. An optional value at a decision point with no
policy in scope is a type error. A policy with no optional value to act on is
a warning.

## Results

- `sum` returns `0` over no values.
- `mean`, `median`, `min`, `max` return `None` over no values; `var` and `sd`
  over fewer than two.
- `n()` counts rows and takes no policy.
- `arrange` sorts missing values last and takes no policy.
- `coalesce`, patterns, `is_some` and `is_none` handle optional values in
  expressions. `x == None` is `None`.
