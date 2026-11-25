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
program. rupam reads the file's schema while checking, through its embedded
DuckDB, so column names and types are known before anything runs. Every
column of such a file is optional (`T?`), because nothing guarantees it is
complete. The generated script checks that the file still has that schema
before reading it, reads 64-bit integers exactly in both dialects, and turns
NaN into a missing value.

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
| `select(a, b)` | keeps and orders columns; also `a:c`, `-a`, `starts_with("x_")`, `where(is_numeric)` |
| `mutate(x = e, y = e2)` | adds or replaces columns in order; `y` can use the new `x` |
| `rename(old = new)` | renames columns |
| `summarize(n = n(), s = sum(x), by = k)` | one row per group with the keys and the summaries |
| `arrange(k, desc(v))` | an explicitly ordered view |
| `take(n)` | first `n` rows of an ordered view; on an unordered table it is an error |
| `distinct(a, b)` | one row per distinct key |
| `join(other, on = k, how = left)` | `inner`, `left`, `right`, `full`, `semi` or `anti`; `on = left.a == right.b` for differing names |
| `pivot_longer(cols = (a, b), names_to = k, values_to = v)` | columns into key/value rows |
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
unrelated tables cannot be combined without a `join`, even if they happen to
have the same length.

## Missing values

Missing is `None`. It is distinct from `NaN`, from zero and from the empty
string, and rupam never fills it in for you.

- Arithmetic on a `T?` gives a `T?`. A scalar function applied to a nullable
  column is skipped on missing rows.
- `coalesce(x, default)` gives a non-optional result.
- `sum`, `mean` and `sd` skip missing values and return `None` when nothing is
  left (or fewer than two values for `sd`). `n()` counts rows and is never
  missing.
- Missing values in a grouping key form one group, and `NaN` keys form another.
- A `filter` whose condition can be missing needs a policy:
  `filter(cost > 0.0, missing = skip)` drops those rows, `missing = propagate`
  keeps them and `missing = error` stops the run.

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
