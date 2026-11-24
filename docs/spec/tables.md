# Tables

A table has named, typed columns in a fixed column order and no row order.
Its type records the columns, which of them are keys, and whether its schema
was validated or inferred:

```text
Table<{customer_id: CustomerId, region: String, revenue: Float64, cost: Float64?}>
```

## Verbs

Verbs are ordinary typed functions from the standard library. Each has a
schema transformer the checker applies statically; none is a separate
language.

| Verb | Meaning |
| --- | --- |
| `mutate(name = e, ...)` | add or replace columns in order; each binding sees the ones before it |
| `filter(predicate)` | keep the rows where the predicate is `TRUE` |
| `select(a, b, new = old)` | keep, reorder and rename columns; selectors below |
| `rename(new = old)` | rename columns |
| `arrange(a, desc(b))` | order rows; makes the table ordered |
| `slice_head(n = 5)` | the first rows of an ordered table; an error on an unordered one |
| `summarize(name = aggregate, ..., by = keys)` | one row per group with the keys and the aggregates; ungrouped gives one row |
| `count(a, b)` | `summarize(n = n(), by = (a, b))` |
| `distinct(a, b)` | one row per distinct key; other columns come from an unspecified row in each group |
| `left_join(y, by = key)` and `inner_join`, `right_join`, `full_join`, `semi_join`, `anti_join` | joins; see below |
| `pivot_longer(cols = (a, b), names_to = k, values_to = v)` | columns into key/value rows |
| `pivot_wider(names_from = k, values_from = v, levels = [a, b])` | the reverse |
| `validate(checks..., warn_above =, stop_above =)` | data checks with thresholds and a report; experimental |

`mutate(..., by = site)` evaluates reductions per group and keeps every row,
so `revenue / mean(revenue)` is relative to each site. `lag`, `lead` and
cumulative functions need an ordered table or an explicit `order_by =`.

Every verb takes `missing =` (see [Missing values](missing.md)).

`group_by` does not exist. Grouping is always explicit at the verb that uses
it, so no grouped state lingers between steps.

## Names inside verbs

Inside a verb's arguments a bare name is a column of the table flowing in.
Outside verbs, names are ordinary bindings. When a name is both a column and
an outer binding, using it bare is an error: write `$name` for the column or
`^name` for the binding.

Functions that take columns need no quoting. A parameter whose type is a
column of the piped table makes bare names at the call site resolve as
columns:

```text
let mean_by(data, value, key) =
  data |> summarize(average = mean(value), by = key)

sales |> mean_by(revenue, region)
```

Calling `mean_by` with a value that is not a column of `sales` is a type
error at the call site.

## Selectors

`select` and the `cols =` arguments of other verbs accept names, ranges
(`region:cost`), exclusions (`-cost`), `starts_with("q_")`, `ends_with`,
`contains`, `matches(r"...")` and `where(is_number)`. Selectors resolve
against the static schema; an unknown name is a compile error with
suggestions.

## Keys and joins

A table's type lists its keys. Keys come from a contract's `primary_key`,
from `distinct(k)` and `summarize(by = k)` on their output, and from
`assert_key(t, k)`, which checks at run time and records the key.

```text
orders |> left_join(customers, by = customer_id)
orders |> left_join(customers, by = (customer_id == id))
```

- Joins never match on shared names implicitly; `by` is required.
- Key types must match exactly, newtypes included.
- If `by` is a key of the right-hand table, the join cannot multiply rows and
  needs nothing more. Otherwise `relationship = one_to_many` (or
  `many_to_one`, `many_to_many`) must be stated, and is checked at run time.
- Columns of an unmatched side become optional.
- A missing key never matches.
- Two non-key columns with the same name are a compile error until renamed.
- Joins produce unordered tables.

## Reshaping

`pivot_longer` requires the gathered columns to share a type. `pivot_wider`
requires `levels`, the output column names, because columns named after
run-time values would make the schema unknowable. A value of `names_from`
outside `levels` is a run-time error.

## Reading and writing

```text
use contract "data-dict.yaml" as shop
let orders = load(shop.orders)

let visits = read_csv("visits.csv", schema = {id: PatientId, site: String, age: Int64?})
let raw = read_parquet("raw.parquet")

write_parquet(summary, "summary.parquet")
write_csv(summary, "summary.csv")
```

- `load` reads a contracted table with validated types ([Contracts](contracts.md)).
- `read_csv` and `read_parquet` with `schema =` check the file against the
  schema and fail when it differs.
- Without `schema =`, the schema is inferred from the file through DuckDB and
  the table's type is marked inferred. Inference never turns `T`/`F` into
  booleans and treats only empty fields as missing. `rupam schema FILE` prints
  the inferred schema as a `schema =` literal to paste in.
- Strict mode rejects writing or compiling from inferred tables unless the
  project allows it.

## Row order

Only `arrange` creates order. `filter`, joins, `summarize`, `distinct` and
pivots return unordered tables. Strings sort by byte order in every dialect;
there is no locale collation. A preview of an unordered table says so.
