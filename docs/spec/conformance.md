# Conformance

The reference evaluator defines the expected answer for small programs. Every
dialect runs every fixture and must agree with it. Unordered tables are
compared as multisets of typed rows, ordered tables in their requested order,
and floating-point reductions within a relative tolerance for summation
order. Schemas, optionality and keys are compared along with values.

Each REPL fixture also checks that the preview equals the compiled script's
output in the same dialect.

## Required cases

| ID | Program | Expected |
| --- | --- | --- |
| C01 | `let square(x) = x * x; square(3)` | `9 : Int64` |
| C02 | `mutate(s = square(revenue))` | element-wise lifting; `s : Float64` |
| C03 | `mutate(s = square(cost))` with `cost : Float64?` | `s : Float64?`; missing stays missing |
| C04 | `mutate(s = coalesce(cost, 0.0))` | `s : Float64` |
| C05 | `mutate(a = revenue + 1.0, b = a * 2.0)` | `b` uses the new `a` |
| C06 | `mutate(revenue = revenue + 1.0)` | replaces `revenue` in place |
| C07 | `filter(cost > 0.0)` | type error naming `cost`; fixed by `missing = skip` |
| C08 | `select(region:cost)` | inclusive range in schema order |
| C09 | `select(typo)` | unknown column, with suggestions |
| C10 | `let revenue = 3.0; mutate(a = revenue)` on a table with `revenue` | ambiguity error; `$revenue` and `^revenue` both accepted |
| C11 | `let standardize(x) = (x - mean(x)) / sd(x)` on a column | whole-column semantics; result optional |
| C12 | combining columns of two unrelated tables | type error until joined |
| C13 | `summarize(s = sum(x), missing = skip)` on an all-missing group | `0` |
| C14 | `summarize(n = n())` on an empty table | one row, `n = 0`; with `by =`, zero rows |
| C15 | left join with missing keys and unmatched rows | missing keys never match; right-hand columns become optional |
| C16 | join with a duplicated non-key column | compile error until renamed |
| C17 | `distinct(x)` with several `None` | with `propagate`, one row for `None` |
| C18 | `arrange(desc(v), id) |> slice_head(n = 5)` | the five largest, ties broken by `id` |
| C19 | `slice_head(n = 5)` on an unordered table | compile error |
| C20 | a contracted source with a missing value in a required column | validation fails before any output, even if a `filter` would drop the row |
| C21 | `range: [0, 100]` with an observed 101 | no failure; `range` is documentation |
| C22 | a source file changed after validation | the cached validation no longer applies |
| C23 | a restricted column | absent from previews, the viewer, errors and output |
| C24 | an unfinished `mutate(z = fn(` | the complete prefix checks and previews; the unfinished stage never runs |
| C25 | `if (x > 0.0) log(x) else None` on a column | never evaluates `log` of a non-positive value, in every dialect |
| C26 | a user function with `match` lifted over a column | lowers to native code; no callback in the generated script |
| C27 | `0.0 / 0.0` in a `mutate` | runtime error naming the expression and row; `ratio(0.0, 0.0)` is `None` |
| C28 | a source file containing NaN | read as `None` |
| C29 | `x == 0.3` with `x : Float64` | type error; `x ~= 0.3` is accepted |
| C30 | `round(2.5)` and `round(-0.5)` | `2.0` and `-0.0` in every dialect |
| C31 | `d"2026-01-31" + months(1)` | `d"2026-02-28"` |
| C32 | comparing `DateTime<"UTC">` with `DateTime<"Europe/Paris">` | type error until `to_zone` |
| C33 | joining `PatientId` against `HospitalId` | type error |
| C34 | `left_join` on a non-key without `relationship` | compile error |
| C35 | `1:0` | empty vector |
| C36 | `[1, 2, 3] + [1, 2]` | length error |
| C37 | `arrange(name)` on mixed-case strings | byte order in every dialect |

## Benchmarks

The [Brown Benchmark for Table Types](https://github.com/brownplt/B2T2) is
the external benchmark for expressiveness and error quality. Coverage is
reported in [the B2T2 datasheet](../b2t2.md), including what is unsupported.

## Usability

The test that matters is whether an R user can open a contracted table,
inspect a missing-value column, add a column, fix a name or type error, join,
summarise, inspect an intermediate result and produce a script, without
annotations, without restarting the REPL after a mistake, and faster than in
R.
