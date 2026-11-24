# Missing values

Element-wise operations carry `None` through without any decision. Some
operations cannot: they have to decide what a missing value means. Those are
**decision points**:

- a `filter` predicate,
- a reduction (`sum`, `mean`, `sd`, `n_distinct`, ...),
- a grouping key (`by =`) or a `distinct` key,
- a join key.

When an optional value reaches a decision point, the program must say what to
do, with one policy:

| Policy | `filter` | reductions | grouping and `distinct` keys | join keys |
| --- | --- | --- | --- | --- |
| `skip` | rows with an unknown predicate are removed | missing values are ignored | rows with a missing key are removed | a missing key never matches |
| `propagate` | rows with an unknown predicate are kept | the result is missing if any input is | missing forms its own group | not allowed |
| `error` | stop if any predicate is unknown | stop if any value is missing | stop if any key is missing | stop if any key is missing |

## Where the policy is stated

From most to least specific:

1. on a single call: `mean(z, missing = propagate)`;
2. on a verb, for every decision point inside that call:
   `summarize(mean_z = mean(z), n = n(), by = site, missing = skip)`;
3. for a whole file: `use missing = skip` at the top.

The most specific statement wins. A decision point that receives an optional
value with no policy in scope is a type error that names the optional input
and lists the three policies. A policy stated where nothing optional reaches a
decision point is a warning, because it does nothing.

The file-level setting is lexical: it applies to that file only, is visible at
its top, and cannot leak into other files. It does not exist in
`rupam.toml`, so a file means the same thing in every project.

## Results

- `sum` with `skip` returns a plain number: the sum of nothing is `0`.
- `mean`, `median`, `sd`, `var`, `min` and `max` return an optional value,
  since they have no value over zero observations (`sd` and `var` need two).
- `n()` counts rows and never needs a policy.
- `arrange` places missing values last and needs no policy.

## Resolving missing values in expressions

`coalesce(x, fallback)`, `match` and `let Some(v) = x else ... in ...` turn an
optional value into a present one anywhere. `is_some` and `is_none` test for
presence. Comparing with `None` using `==` gives `None` and is not a test.
