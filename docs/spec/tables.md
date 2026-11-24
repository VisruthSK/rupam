# Tables

A table has named, typed columns in a fixed order and unordered rows. Its
type lists the columns, the keys, and whether the schema is validated or
inferred.

## Verbs

| Verb | Result |
| --- | --- |
| `mutate(name = e, ...)` | adds or replaces columns in order; each binding sees earlier ones |
| `filter(predicate)` | rows where the predicate is `TRUE` |
| `select(a, b, new = old)` | the selected columns, in order, renamed as given |
| `rename(new = old)` | the table with columns renamed |
| `arrange(a, desc(b))` | the rows ordered; the result is an ordered table |
| `slice_head(n = 5)` | the first rows of an ordered table |
| `summarize(name = e, ..., by = keys)` | one row per group: keys, then summaries; one row when ungrouped |
| `count(a, b)` | `summarize(n = n(), by = (a, b))` |
| `distinct(a, b)` | one row per distinct key |
| `left_join`, `inner_join`, `right_join`, `full_join`, `semi_join`, `anti_join` | joins |
| `pivot_longer(cols = (a, b), names_to = k, values_to = v)` | columns to key/value rows |
| `pivot_wider(names_from = k, values_from = v, levels = [a, b])` | key/value rows to columns |
| `validate(checks..., warn_above = r, stop_above = r)` | the table, after checking rows against thresholds; experimental |

- `mutate(..., by = keys)` evaluates reductions per group and keeps every row.
- `lag`, `lead` and cumulative functions require an ordered table or
  `order_by =`.
- Every verb accepts `missing =`.
- Grouping exists only as the `by =` argument of the verb that uses it.
- `slice_head` on an unordered table is a type error.
- In `distinct`, non-key columns come from an unspecified row of each group.

## Column names

- Inside a verb's arguments, a bare name is a column of the input table.
- Outside verb arguments, a bare name is a binding.
- A name that is both a column and a binding is an error inside a verb;
  `$name` selects the column and `^name` the binding.
- A function parameter whose type is a column of the piped table resolves
  bare names at the call site as columns:

```text
let mean_by(data, value, key) = data |> summarize(average = mean(value), by = key)

sales |> mean_by(revenue, region)
```

## Selectors

Names, ranges (`region:cost`), exclusions (`-cost`), `starts_with`,
`ends_with`, `contains`, `matches(r"...")` and `where(is_number)`. Selectors
resolve against the schema during checking.

## Keys and joins

Keys come from a contract's `primary_key`, from `distinct(k)` and
`summarize(by = k)` on their outputs, and from `assert_key(t, k)`, which is
checked at run time.

```text
orders |> left_join(customers, by = customer_id)
orders |> left_join(customers, by = (customer_id == id))
```

- `by` is required.
- Key types must be identical.
- A join whose `by` is not a key of the right-hand table requires
  `relationship = one_to_many | many_to_one | many_to_many`, checked at run
  time.
- Columns from an unmatched side become optional.
- Non-key columns with the same name are a type error.
- The result is unordered.

## Reshaping

- `pivot_longer` requires the selected columns to share a type.
- `pivot_wider` requires `levels`. A `names_from` value outside `levels` is a
  run-time error.

## Reading and writing

```text
use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
let visits = read_csv("visits.csv", schema = {id: PatientId, site: String, age: Int64?})
let raw = read_parquet("raw.parquet")
write_parquet(summary, "summary.parquet")
write_csv(summary, "summary.csv")
```

- `load` reads a contracted table ([Contracts](contracts.md)).
- `schema =` checks the file against the given schema.
- Without `schema =` the schema is inferred and the table is marked inferred.
  Inference reads only empty fields as missing and never reads `T` or `F` as
  booleans.
- Reading converts NaN in floating-point columns to `None` and rejects
  infinite values.
- Writing or compiling from an inferred table is a strict-mode error.

## Order

Only `arrange` produces an ordered table. Strings sort by byte order.
