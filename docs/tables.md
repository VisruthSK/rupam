# Tables

A rupam table has named, typed columns and no row order. You get one by
loading a source described in a [data-dict](https://data-dict.tidyverse.org/)
contract, transform it with a small set of verbs, and write it out. The checker
knows the schema at every step, so a typo or a nullable column in the wrong
place fails before any data is read.

## Reading files

```text
let visits = read_csv("visits.csv")
let scores = read_parquet("scores.parquet")
```

`read_csv`, `read_tsv` and `read_parquet` take a path relative to the
program. While checking, rupam reads the whole file through its embedded
DuckDB, so column names, types and optionality are known before anything
runs: a column is `T?` only if the file has missing values in it. The
generated script checks that the file still matches before reading it, reads
64-bit integers exactly in both dialects, and turns NaN into a missing
value.

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
| `filter(pred, missing = skip)` | keeps rows where `pred` is `TRUE`; `missing` says what happens where `pred` is missing |
| `select(a, b, new = old)` | keeps, orders and renames columns; also `a:c`, `-a`, `starts_with("x_")`, `where(is_numeric)` |
| `mutate(x = e, y = e2)` | adds or replaces columns in order; `y` can use the new `x` |
| `rename(new = old)` | renames columns |
| `summarize(n = n(), s = sum(x), by = k)` | one row per group with the keys and the summaries |
| `arrange(k, desc(v))` | an explicitly ordered view |
| `slice_head(n = 5)` | the first rows of an arranged table; on an unordered table it is an error |
| `count(a, b)` | `summarize(n = n(), by = [a, b])` |
| `distinct(a, b)` | one row per distinct key |
| `left_join(other, by = k, relationship = many_to_one)` | also `inner_join`, `right_join`, `full_join`, `semi_join` and `anti_join`; `by = (a == b)` for differing names |
| `pivot_longer(cols = [a, b], names_to = k, values_to = v)` | columns into key/value rows |
| `pivot_wider(names_from = k, values_from = v, levels = [a, b])` | the reverse; the output columns must be listed |
| `print(t)` | shows the table; in a script, prints it |
| `write_parquet(t, "out.parquet")` | writes the result |

`mutate(..., by = region)` evaluates aggregates per group but keeps every row,
which is how you compute a value relative to its group:

```text
orders |> mutate(relative = revenue / mean(revenue), by = region)
```

`pivot_wider` needs `levels` because a column named after a runtime value
would make the schema unknowable until the data is read.

## Names inside verbs

Inside a verb's arguments, a bare name refers to a column if the table has one,
and to an ordinary binding otherwise. `$name` always means the column and
`^name` always means the outer binding:

```text
let revenue = 999.0
orders |> mutate(ratio = $revenue / ^revenue)
```

## Functions over columns

You don't write separate scalar and column versions of a function. The checker
works out how each call is used:

```text
let square(x) = x * x
let standardize(x) = (x - mean(x)) / sd(x)

orders |> mutate(a = square(revenue), z = standardize(revenue))
```

Code inside a verb is checked by the same type checker as the rest of the
language, so anything you can write at the top level works on columns:
`if`, `match`, `let ... in`, your own functions and the standard library
(`abs`, `sqrt`, `log`, `round`, `pmin`, `coalesce`, `some`, `str_to_lower`,
`str_detect`, `mean`, `median`, `sd`, `min`, `max`, `n_distinct`, ...).

`square` is scalar, so it is applied row by row. `standardize` calls the
aggregates `mean` and `sd`, so it sees the whole column (or the whole group
under `by`), and their results are broadcast back to each row. Columns from two
unrelated tables cannot be combined without a join, even if they happen to
have the same length.

## Missing values

Missing is `None`. It is distinct from `NaN`, from zero and from the empty
string, and rupam never fills it in for you.

Arithmetic, comparisons and function calls on a `T?` give a `T?` that is
missing where the input is. `coalesce(x, default)` gives a non-optional
result, and `filter(some(x))` makes `x` non-optional in the rows that remain.

Wherever a missing value would change the answer, you choose what happens:

```text
orders |> filter(cost > 0.0, missing = skip)
orders |> summarize(average = mean(cost), by = region, missing = skip)
orders |> summarize(average = mean(cost, missing = propagate), by = region)
```

`skip` leaves missing values out, `propagate` makes the result missing, and
`error` stops the run. A call's own `missing =` wins over its verb's, and a
verb's wins over a file-wide default:

```text
use missing = skip
```

Grouping keys that can be missing take `group_missing =` the same way:
`skip` leaves those rows out of `summarize` (and gives `None` for
group-dependent `mutate` results), `propagate` makes missing one group.
Without a policy, a filter, reduction or grouping on a column that can be
missing is a type error.

`sum`, `mean`, `median`, `min` and `max` return `None` when there are no
values left, and `var` and `sd` when there are fewer than two. `n()` counts
rows and is never missing.

## Generated code

The `r-dplyr` dialect emits dplyr; `python-polars` emits a Polars lazy query. Both reproduce the semantics above, not their library's defaults: R's
`sum` of nothing is 0 and Polars evaluates both arms of `when/then`, so the
generated code guards those cases. When a construct has no faithful native
form (checked `Int64` arithmetic in Polars, for example) the Python target
falls back to a row callback, and `:explain` says so. If neither path is
faithful, compilation fails and names the construct.

Every generated script validates its contracted inputs first and needs only
its target libraries and the data-dict CLI to run.

Julia is the planned third target.
