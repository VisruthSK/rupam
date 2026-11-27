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
program. While checking, rupam reads the whole file with its own readers,
so column names, types and optionality are known before anything
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
footer for physical types. A column marked `required` or `primary_key` is
typed `T`; anything else is `T?`, even if the current file happens to have no
gaps. The `primary_key` columns together form the table's key, and each
`unique` column is a key among its present values. `range`, `examples` and
descriptions are documentation only and never become checks.

Type checking needs only the contract and the Parquet footer. Columns marked
`display: restricted` are left out of previews and generated output.

## Locks

An analysis is compiled for particular data. `rupam lock shop.rp` validates
every contract with `data-dict validate-data`, so a bad row fails even if a
later `filter` would have dropped it, and records the SHA-256 of every input
in `rupam.lock`, together with the dialect, the settings and the schemas rupam
inferred for files without a contract. Editing the program needs no relock
unless it starts reading a new file. `compile` and `run` need a current lock. They
create the first one themselves when nothing needs accepting, but a program
that reads a CSV or Parquet file without a contract must be locked
explicitly, after you have looked at the inferred schemas.

Generated scripts read each input's bytes once, check them against the
locked hash and parse those same bytes, so they need neither rupam nor the
data-dict CLI. When an input changes, the script, `compile` and `run` all
stop and ask you to run `rupam lock` again.

## Verbs

| Verb | What it does |
| --- | --- |
| `filter(pred, missing = skip)` | keeps rows where `pred` is `TRUE`; `missing` says what happens where `pred` is missing |
| `select(a, b, new = old)` | keeps, orders and renames columns; also `a:c`, `-a`, `starts_with("x_")`, `where(is_numeric)` |
| `mutate(x = e, y = e2)` | adds or replaces columns in order; `y` can use the new `x` |
| `rename(new = old)` | renames columns |
| `summarize(n = n(), s = sum(x), by = k)` | one row per group with the keys and the summaries |
| `arrange(k, desc(v))` | an explicitly ordered view |
| `slice_head(n = 5)` | the first rows of a table arranged by columns that include a key |
| `count(a, b)` | `summarize(n = n(), by = [a, b])` |
| `distinct(a, b)` | one row per distinct combination; `a, b` becomes a key |
| `assert_key(a, b)` | checks when it runs that `a, b` identifies each row, and makes it a key |
| `left_join(other, by = k)` | also `inner_join`, `right_join`, `full_join`, `semi_join` and `anti_join`; `by = (a == b)` for differing names |
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

## Keys and joins

rupam tracks which columns identify a row. Keys come from contracts,
`distinct`, `summarize(by = ...)`, `count` and `assert_key`, and survive the
verbs that keep them intact: `filter`, `arrange` and `slice_head` keep every
key, `select` and `rename` keep keys whose columns survive, and `mutate`
drops a key when it replaces one of its columns.

A join has to know how many rows can match. When a key proves it, as when
joining onto a summary by its grouping column, nothing more is needed:

```text
let totals = orders |> summarize(total = sum(revenue), by = region)
orders |> left_join(totals, by = region)
```

Otherwise say so with `relationship = one_to_one`, `many_to_one`,
`one_to_many` or `many_to_many`, which both dialects check when the join
runs. `unmatched = error` also requires every row to find a match: the left
side for `left_join`, the right side for `right_join`, both for `inner_join`
and `full_join`. Missing keys never match.

`slice_head` needs a total order, so the table must be arranged by columns
that include a key. Ties have no hidden tie-breaker.

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
orders |> summarize(average = mean(cost, missing = keep), by = region)
```

`skip` leaves missing values out, `keep` makes the result missing, and
`error` stops the run. A call's own `missing =` wins over its verb's, and a
verb's wins over a file-wide default:

```text
set missing = skip
```

Grouping keys that can be missing take `group_missing =` the same way:
`skip` leaves those rows out of `summarize` (and gives `None` for
group-dependent `mutate` results), `keep` makes missing one group.
Without a policy, a filter, reduction or grouping on a column that can be
missing is a type error.

`sum`, `mean`, `median`, `min` and `max` return `None` when there are no
values left, and `var` and `sd` when there are fewer than two. `n()` counts
rows and is never missing.

## Generated code

The `r-dplyr` dialect emits dplyr; `python-polars` emits a Polars lazy query. Both reproduce the semantics above, not their library's defaults: R's
`sum` of nothing is 0 and Polars evaluates both arms of `when/then`, so the
generated code guards those cases. When a construct has no faithful native
form (checked `Int` arithmetic in Polars, for example) the Python target
falls back to a row callback, and `:explain` says so. If neither path is
faithful, compilation fails and names the construct.

Every generated script validates its contracted inputs first and needs only
its target libraries and the data-dict CLI to run.

Julia is the planned third target.
