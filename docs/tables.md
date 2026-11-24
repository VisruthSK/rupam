# Tables

A rupam table has named, typed columns and no row order. You get one by
loading a source described in a [data-dict](https://data-dict.tidyverse.org/)
contract, transform it with a small set of verbs, and write it out. The checker
knows the schema at every step, so a typo or a nullable column in the wrong
place fails before any data is read.

## Contracts

```text
use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
```

rupam asks the data-dict CLI for the resolved contract and reads the Parquet
footer for physical types. A column marked `required` is typed `T`. Anything
else is `T?`, even if the current file happens to have no gaps. `range`,
`examples` and descriptions are documentation only and never become checks.

Type checking needs only the contract and the Parquet footer. Before any
preview, save or generated script touches values, the whole source is
validated with `data-dict validate-data`, so a bad row fails the run even if a
later `filter` would have dropped it. Columns marked `display: restricted` are
left out of previews and generated output.

## Verbs

| Verb | What it does |
| --- | --- |
| `filter(pred)` | keeps rows where `pred` is `true`; `pred` must be a non-optional `Bool` |
| `select(a, b)` | keeps and orders columns; also `a:c`, `-a`, `starts_with("x_")`, `where(is_numeric)` |
| `derive(x = e, y = e2)` | adds columns in order; `y` can use `x`; existing names are an error |
| `replace(x = e)` | overwrites existing columns; unknown names are an error |
| `rename(old = new)` | renames columns |
| `summarize(n = count(), s = sum(x), by = k)` | one row per group with the keys and the summaries |
| `arrange(k, desc(v))` | an explicitly ordered view |
| `take(n)` | first `n` rows of an ordered view; on an unordered table it is an error |
| `distinct(a, b)` | one row per distinct key |
| `join(other, on = k, how = left)` | `inner`, `left`, `right`, `full`, `semi` or `anti`; `on = left.a == right.b` for differing names |
| `pivot_longer(cols = (a, b), names_to = k, values_to = v)` | columns into key/value rows |
| `pivot_wider(names_from = k, values_from = v, levels = [a, b])` | the reverse; the output columns must be listed |
| `save_parquet(t, "out.parquet")` | writes the result |

`derive(..., by = region)` evaluates aggregates per group but keeps every row,
which is how you compute a value relative to its group:

```text
orders |> derive(relative = revenue / mean(revenue), by = region)
```

`pivot_wider` needs `levels` because a column named after a runtime value
would make the schema unknowable until the data is read.

## Names inside verbs

Inside a verb's arguments, a bare name refers to a column if the table has one,
and to an ordinary binding otherwise. `$name` always means the column and
`^name` always means the outer binding:

```text
let revenue = 999.0
orders |> derive(ratio = $revenue / ^revenue)
```

## Functions over columns

You don't write separate scalar and column versions of a function. The checker
works out how each call is used:

```text
let square(x) = x * x
let standardize(x) = (x - mean(x)) / sd(x)

orders |> derive(a = square(revenue), z = standardize(revenue))
```

`square` is scalar, so it is applied row by row. `standardize` calls the
aggregates `mean` and `sd`, so it sees the whole column (or the whole group
under `by`), and their results are broadcast back to each row. Columns from two
unrelated tables cannot be combined without a `join`, even if they happen to
have the same length.

## Missing values

Missing is `None`. It is distinct from `NaN`, from zero and from the empty
string, and rupam never fills it in for you.

- Arithmetic on a `T?` gives a `T?`. A scalar function applied to a nullable
  column is skipped on missing rows.
- `coalesce(x, default)` gives a non-optional result.
- `sum`, `mean` and `sd` skip missing values and return `None` when nothing is
  left (or fewer than two values for `sd`). `count()` counts rows and is never
  missing.
- Missing values in a grouping key form one group, and `NaN` keys form another.
- `filter` rejects an optional predicate. Write
  `filter(coalesce(cost > 0.0, false))` and say what missing should mean.

## Generated code

`rupam compile r` emits dplyr; `rupam compile python` emits a Polars lazy
query. Both reproduce the semantics above, not their library's defaults: R's
`sum` of nothing is 0 and Polars evaluates both arms of `when/then`, so the
generated code guards those cases. When a construct has no faithful native
form (checked `Int64` arithmetic in Polars, for example) the Python target
falls back to a row callback, and `:explain` says so. If neither path is
faithful, compilation fails and names the construct.

Every generated script validates its contracted inputs first and needs only
its target libraries and the data-dict CLI to run.

Julia is the planned third target.
