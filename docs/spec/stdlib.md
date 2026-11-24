# Standard library

Every function here works on single values, vectors and columns according to
the lifting rules in [Types](types.md), and lowers to native vectorised code
in every dialect. Reductions take `missing =` when their input is optional.

## Reductions

| Function | Result |
| --- | --- |
| `n()` | rows in the table or group |
| `n_distinct(x)` | distinct values |
| `sum(x)` | sum; `0` over nothing |
| `prod(x)` | product, always `Float64` |
| `mean(x)`, `median(x)` | optional over nothing |
| `var(x)`, `sd(x)` | sample variance and standard deviation (divisor `n - 1`); optional under two values |
| `min(x)`, `max(x)` | optional over nothing |
| `quantile(x, p)` | linear interpolation (R type 7, NumPy `linear`); `method =` selects another |
| `mad(x)`, `iqr(x)` | robust spread |
| `any(x)`, `all(x)` | over `Bool` |
| `first(x)`, `last(x)` | ordered tables only |

## Column transforms

Whole-vector functions that return a vector of the same length:

| Function | Result |
| --- | --- |
| `center(x)` | `x - mean(x)` |
| `scale(x)` | `x / sd(x)` |
| `standardize(x)` | `(x - mean(x)) / sd(x)`; `method = robust` uses median and MAD |
| `rescale(x)` | `(x - min(x)) / (max(x) - min(x))`, into 0 to 1; `to = (a, b)` for another range |
| `rank(x, ties = average)` | ranks; `ties` is `average`, `min`, `max` or `dense` |
| `percent_rank(x)`, `ntile(x, n)` | relative position, buckets |
| `cumsum(x)`, `cummean(x)`, `lag(x)`, `lead(x)` | need an ordered table or `order_by =` |

Under `by =` each of these applies within each group.

## Element-wise numbers

`abs`, `sign`, `sqrt`, `exp`, `log`, `log2`, `log10`, `floor`, `ceiling`,
`trunc`, `round(x, digits, ties = even)`, `pmin(a, b)`, `pmax(a, b)`,
`clamp(x, lo, hi)`, `float`, `int`, `ratio(a, b)`, `div(a, b)`.

`min` and `max` always reduce; `pmin` and `pmax` are the element-wise
versions. Mixing them up is a type error, since a reduction returns a single
value where a vector is expected.

## Missing values

`coalesce(x, fallback)`, `is_some(x)`, `is_none(x)`.

## Strings

`str_length`, `str_to_lower`, `str_to_upper`, `str_trim`, `str_pad`,
`str_sub`, `str_starts`, `str_ends`, `str_detect(x, r"...")`,
`str_extract`, `str_extract_all`, `str_replace`, `str_replace_all`,
`str_split`. Concatenation is interpolation: `"{first} {last}"`.

## Regex

rupam has one small regex dialect that every target engine supports
identically: literals, `.`, character classes and ranges, `\d \w \s` and
their negations, anchors `^ $`, groups and alternation, non-capturing groups,
named captures, and the quantifiers `* + ? {m,n}` with lazy variants.
Backreferences and lookaround are not included, because the engines disagree
on them. `r"..."` literals are parsed when the program is checked. The REPL
can preview a pattern's matches against a column (see [Tooling](tooling.md)).

## Dates and times

`today()` and `now()` are effects. `year`, `month`, `day`, `wday`, `yday`,
`hour`, `minute`, `second`, `floor_date(x, unit)`, `to_zone(x, zone)`,
`assume_zone(x, zone)`, `days`, `hours`, `minutes`, `seconds` (durations),
`months`, `years` (periods), `between(x, lo, hi)`.

## Vectors

`length`, `rev`, `sort`, `unique`, `map(xs, f)`, `keep(xs, f)`,
`reduce(xs, init, f)`, `seq(from, to, by)`, `rep(x, times)`, `x %in% set`.
