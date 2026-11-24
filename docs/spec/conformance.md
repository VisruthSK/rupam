# Conformance

Every dialect runs every fixture and must match the reference evaluator.
Unordered tables compare as multisets of typed rows; ordered tables in order;
floating-point reductions within a relative tolerance for summation order.
Schemas, optionality, refinements and keys are compared with values. Each
fixture also compares the REPL preview with the compiled script's output.

## Required cases

| ID | Program | Expected |
| --- | --- | --- |
| C01 | `let square(x) = x * x; square(3)` | `9 : Int64` |
| C02 | `mutate(s = square(revenue))` | element-wise; `s : Float64` |
| C03 | `mutate(s = square(cost))`, `cost : Float64?` | `s : Float64?` |
| C04 | `mutate(s = coalesce(cost, 0.0))` | `s : Float64` |
| C05 | `mutate(a = revenue + 1.0, b = a * 2.0)` | `b` uses the new `a` |
| C06 | `mutate(revenue = revenue + 1.0)` | `revenue` replaced in place |
| C07 | `filter(cost > 0.0)` | type error naming `cost`; accepted with `missing = skip` |
| C08 | `select(region:cost)` | inclusive range in schema order |
| C09 | `select(typo)` | unknown column, with suggestions |
| C10 | `let revenue = 3.0` then `mutate(a = revenue)` on a table with `revenue` | ambiguity error; `$revenue` and `^revenue` accepted |
| C11 | `standardize(revenue)` | whole-column; `Float64?` |
| C12 | columns of two unrelated tables combined | type error |
| C13 | `summarize(s = sum(x), missing = skip)` on an all-missing group | `0.0` |
| C14 | `summarize(n = n())` on an empty table | one row, `n = 0`; with `by =`, zero rows |
| C15 | `left_join` with missing keys and unmatched rows | missing keys never match; right columns optional |
| C16 | join with a duplicated non-key column | type error |
| C17 | `distinct(x, missing = propagate)` with several `None` | one `None` row |
| C18 | `arrange(desc(v), id) |> slice_head(n = 5)` | five largest, ties by `id` |
| C19 | `slice_head(n = 5)` on an unordered table | type error |
| C20 | contracted source with `None` in a required column | validation fails before output |
| C21 | `range: [0, 100]` with a value of 101 | no failure |
| C22 | source changed after validation | revalidated |
| C23 | restricted column | absent from previews, viewer, diagnostics, output |
| C24 | unfinished `mutate(z = fn(` | prefix checks and previews; unfinished stage never runs |
| C25 | `if (x > 0.0) log(x) else None` | type-checks; `log` receives only positive values |
| C26 | user function with `match` lifted over a column | native code, no callback |
| C27 | `mutate(rate = cases / population)` | `zero-divisor` error; accepted after `filter(population > 0)` or as `ratio(cases, population)` |
| C28 | source file with NaN in a float column | read as `None` |
| C29 | source file with infinity in a float column | read fails |
| C30 | `x == 0.3`, `x : Float64` | type error; `x ~= 0.3` accepted |
| C31 | `round(2.5)`, `round(-0.5)` | `2.0`, `-0.0` |
| C32 | `d"2026-01-31" + months(1)` | `d"2026-02-28"` |
| C33 | `DateTime<"UTC">` compared with `DateTime<"Europe/Paris">` | type error |
| C34 | `PatientId` joined against `HospitalId` | type error |
| C35 | `left_join` on a non-key without `relationship` | type error |
| C36 | `1:0` | empty vector |
| C37 | `[1, 2, 3] + [1, 2]` | length error |
| C38 | `arrange(name)` on mixed-case strings | byte order |
| C39 | `match bmi { ..<18.5 => Under, 18.5..<25.0 => Healthy, 25.0.. => Over }` on a column | enum column; exhaustive without `_` |
| C40 | `Int64(x)` with `x : Float64`; `Int64(round(x))` | type error; accepted |

## Benchmarks

The [Brown Benchmark for Table Types](https://github.com/brownplt/B2T2), with
coverage reported in [the B2T2 datasheet](../b2t2.md).
