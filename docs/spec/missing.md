# Missing values

A **decision point** is a `filter` predicate, a reduction, a transform that
reduces or ranks its input, a grouping or `distinct` key, or a join key. An
optional value at a decision point requires a policy:

| Policy | `filter` | reductions | grouping, `distinct` | join keys |
| --- | --- | --- | --- | --- |
| `skip` | drop rows with an unknown predicate | ignore missing values | exclude rows with a missing key from grouping or distinctness | missing never matches |
| `propagate` | keep rows with an unknown predicate | result is `None` if any input is missing | missing is one group | not allowed |
| `error` | fail on an unknown predicate | fail on a missing value | fail on a missing key | fail on a missing key |

`mutate` always preserves rows. With effective grouping policy `skip`, group-dependent
results for rows excluded by missing keys are `None`; independent pointwise
expressions still evaluate. `summarize` excludes those rows entirely. Join
preservation of unmatched rows follows the join kind.

## Scope

An explicit policy on a decision point or verb is lexical. A file directive
`use missing = skip` supplies a lexical default for subsequent definitions
and expressions. An explicitly set policy, including a definition's lexical
file default, remains fixed when a function is passed or called elsewhere.
A helper defined without a policy for a required decision point carries an
unresolved requirement in its inferred scheme. At a call site, supply that
requirement from an explicit helper-call policy, the enclosing verb policy,
or the caller's lexical file default, in that order. Requirements propagate
through nested and higher-order calls. Unresolved decision points on optional
inputs are errors. Policies already fixed inside a helper are never overridden
by callers. Explicit policies at inner decision points override their own
lexical defaults.

`group_missing = skip | propagate | error` on grouped verbs overrides the
policy for grouping keys only. `missing =` remains the common convenience
policy for both reductions and grouping when no group-specific policy is set.
A grouping decision does not supply a reduction policy on its own. For example:

```text
data |> summarize(average = mean(x), by = region,
                  missing = skip, group_missing = propagate)
```

Here missing regions form a group and missing measurements are excluded from
its mean. A file may specify `use group_missing = propagate` independently;
explicit verb `group_missing` takes precedence, then an explicit common verb
policy, then the lexical group default, then the lexical common policy. The
more local explicit common policy therefore overrides outer defaults. Contextual grouping requirements inherit
through helpers only when unresolved, just as reduction requirements do.
A verb-level or call-level `group_missing` is invalid without grouping;
file defaults are ignored where not applicable. `distinct` uses `missing` directly;
joins accept only `skip` and `error` for missing keys. The policy table defines
how the common vocabulary expands by operation; no new synonyms are needed.
An explicitly selected policy or a policy that proves unnecessary is not a warning.
Grouped mutation results on excluded rows are optional even when their reduction
would otherwise be guaranteed present.

## Results

- `sum`, `mean`, `median`, `min`, `max`, `quantile`, `mad` and `iqr` return
  `None` over no present values, including an empty input and an all-missing
  input with `skip`. `var` and `sd` return `None` under two present values.
  `coalesce(sum(x), 0)` deliberately supplies the additive identity.
- `prod` also returns `None` over no present values. `n()` counts rows and
  takes no policy. `n_distinct` counts present values under `skip`.
- `any` and `all` reduce present values under `skip` (empty identities
  `FALSE` and `TRUE`); `propagate` uses three-valued Boolean reduction: a
  decisive present value determines the result, otherwise a missing input
  yields `None`. `error` rejects missing values before reduction.
- Infer a present result if the operator's minimum present count and domain
  requirements are established (e.g. `sum` of a required column in a nonempty
  group). The result type never changes in response to actual values.
- Length-preserving transforms retain missing positions. For reductions used
  in transforms, `skip` uses present observations; `propagate` makes an
  affected group's reduction missing. Positional `lag` and `lead` do not skip
  over missing positions; see [Standard library](stdlib.md#transforms).
- `arrange` sorts missing values last in either direction and takes no policy.
- `coalesce`, patterns, `some` and `none` handle optional values in expressions.
  A direct comparison to literal `None` is rejected.
