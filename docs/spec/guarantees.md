# Guarantees

rupam's design removes whole classes of bugs that R and pandas users meet
regularly. Each row names the class, where it is documented, and the rule that
makes it impossible. Sources: Patrick Burns, *The R Inferno* (section numbers
below), the R FAQ, and the pandas and Polars user guides.

## Removed by types and semantics

| Bug class | Source | Why it cannot happen |
| --- | --- | --- |
| `c()` and comparison coercion (`"10" < 9`) | Inferno 8.1.8–9 | vectors are homogeneous; no implicit conversion |
| recycling; `x == c(1, 2)` used as membership | 8.1.6, 8.1.31 | only single values broadcast; `%in%` is separate |
| `1:length(x)` on an empty vector gives `c(1, 0)` | 8.1.65 | ranges never count down |
| negative-nothing indexing drops everything | 8.1.13 | no negative indexing |
| missing values in subscripts | 8.2.14–15 | indexes are `Int64`, never optional |
| `ifelse` dropping attributes, dates turning into numbers | 8.1.28, 8.2.7 | types are preserved by every operation |
| dropped dimensions, `drop = FALSE` | FAQ 7.5, 8.1.44 | no matrices; a column becomes a vector only on request |
| `sapply` simplification, `apply` on data frames | 8.3.20, 8.2.38 | the result type of `map` is known before it runs |
| factor codes, combining and dropped levels | FAQ 7.10, 8.2.1–8 | no factors; categories are enums |
| `max` vs `pmax` | 8.2.22 | reductions and element-wise versions have different types |
| `NA == NA`, `if (NA)`, NA vs NaN vs NULL | 8.1.4, 8.2.11, 8.2.16 | one `None`; `if` needs `Bool` |
| partial argument and `$` matching | 8.1.20 | exact names only |
| `T`/`F` reassignment, `=` vs `<-`, `=` vs `==` in `subset` | 8.2.26, 8.2.32 | `TRUE`/`FALSE` are keywords; `=` only binds or names |
| `if` on a vector, `&` vs `&&` | 8.2.17 | `if` needs one `Bool` |
| newline before `else` | 8.1.43 | a line starting with `else` continues |
| `-2^2` | FAQ 7.33 | `-x^y` is a parse error |
| lazy arguments, `<<-`, accidental globals | 8.3.14–19 | immutable values, lexical scope |
| row names, `merge` reordering rows | 8.1.46 | no row names; tables are unordered until `arrange` |
| `is.integer(1)` is `FALSE` | 8.2.19 | `1` is `Int64`, `1.0` is `Float64` |
| floating-point `==` | FAQ 7.31 | `Float64` is not equatable; `~=` is explicit |
| rounding differing between tools | 8.1.52 | one rule, IEEE half-to-even, in every dialect |
| variance divisor differs between libraries | pandas vs NumPy | `var` and `sd` always divide by `n - 1` |
| data masking: a column hides a variable | 8.1.32 | ambiguous bare names are an error |
| views, copies, chained assignment, `inplace` | pandas | values are immutable |
| integer column becomes float when a value is missing | pandas | `Int64?` is native |
| index alignment; `in` tests the index | pandas | no index; joins are explicit |
| mutable default arguments, late-binding closures | Python | immutable values; closures capture values |
| "truth value of a Series is ambiguous" | pandas | `if` needs one `Bool` |
| groupby silently dropping missing keys; all-missing sums shown as 0 | pandas | the missing-value policy is required |
| null vs NaN | Polars | there is no NaN |
| time-zone mixing, `Date - Date` as a number, naive date-times | lubridate, pandas | zones in the type, `Duration`, no naive date-times |
| `+ months(1)` returning `NA` at month end | lubridate | `Period` arithmetic clamps to month end |
| join row explosions | dplyr, pandas | keys in table types; `relationship` otherwise |
| strings sorting differently by locale | R, Polars | byte order everywhere |
| REPL state disagreeing with the script | all | the REPL runs the compiled program |

## Reduced, not removed

- **Guessed column types when reading files.** A contract or an inline
  `schema =` removes guessing entirely. Without one, the inferred schema is
  printed, marked inferred in the type, and never reads `T`/`F` as booleans.
- **Binary representation of decimals.** `round(0.15, 1)` follows the stored
  binary value. A decimal type is deferred (see [Future](future.md)).
- **`Int64` overflow.** Out of range results are unspecified. The values that
  reach that range in practice (timestamps, identifiers, products) are dates,
  newtypes without arithmetic and `Float64` products, so ordinary programs do
  not get there.
