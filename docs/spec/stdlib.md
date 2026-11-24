# Standard library

All functions lift per [Types](types.md) and lower to native vectorised code
in every dialect. Reductions take `missing =` when their input is optional.

## Reductions

| Function | Result |
| --- | --- |
| `n()` | row count of the table or group |
| `n_distinct(x)` | distinct values |
| `sum(x)` | sum; `0` over no values |
| `prod(x)` | product, `Float64` |
| `mean(x)`, `median(x)` | `None` over no values |
| `var(x)`, `sd(x)` | sample statistics (divisor `n - 1`); `None` under two values |
| `min(x)`, `max(x)` | `None` over no values |
| `quantile(x, p)` | linear interpolation; `method =` selects another |
| `mad(x)`, `iqr(x)` | robust spread |
| `any(x)`, `all(x)` | over `Bool` |
| `first(x)`, `last(x)` | ordered tables only |

## Transforms

Return a vector of the input's length. Under `by =` they apply per group.

| Function | Result |
| --- | --- |
| `center(x)` | `x - mean(x)` |
| `scale(x)` | `x / sd(x)` |
| `standardize(x)` | `(x - mean(x)) / sd(x)`; `method = robust` uses median and MAD |
| `rescale(x)` | `(x - min(x)) / (max(x) - min(x))`; `to = (a, b)` sets the range |
| `rank(x, ties = average)` | `ties` is `average`, `min`, `max` or `dense` |
| `percent_rank(x)`, `ntile(x, n)` | relative rank, bucket |
| `cut(x, at = [...], into = (...))` | the enum value of the bin containing `x`; `n` breakpoints give `n + 1` bins |
| `cumsum(x)`, `cummean(x)`, `lag(x)`, `lead(x)` | ordered input |

`scale`, `standardize` and `rescale` return `None` for every element when the
divisor is zero or undefined.

## Numbers

`abs`, `sign`, `sqrt`, `exp`, `log`, `log2`, `log10`, `floor`, `ceiling`,
`trunc`, `round(x, digits, ties = even)`, `pmin(a, b)`, `pmax(a, b)`,
`clamp(x, lo, hi)`, `float`, `int`, `div`, `ratio`, `positive`,
`nonnegative`, `nonzero`. `min` and `max` reduce; `pmin` and `pmax` are
element-wise.

## Missing values

`coalesce(x, fallback)`, `is_some(x)`, `is_none(x)`.

## Strings

`str_length`, `str_to_lower`, `str_to_upper`, `str_trim`, `str_pad`,
`str_sub`, `str_starts`, `str_ends`, `str_detect`, `str_extract`,
`str_extract_all`, `str_replace`, `str_replace_all`, `str_split`.
Concatenation is interpolation.

## Regex

Literal characters, `.`, character classes and ranges, `\d \w \s` and
negations, `^ $`, groups, non-capturing groups, named groups, alternation,
and `* + ? {m,n}` with lazy forms. No backreferences or lookaround. Patterns
are validated during checking.

## Dates and times

`today()`, `now()` (effects), `year`, `month`, `day`, `wday`, `yday`, `hour`,
`minute`, `second`, `floor_date(x, unit)`, `to_zone`, `assume_zone`,
`days`, `hours`, `minutes`, `seconds` (durations), `months`, `years`
(periods), `between(x, lo, hi)`.

## Vectors

`length`, `rev`, `sort`, `unique`, `map(xs, f)`, `keep(xs, f)`,
`reduce(xs, init, f)`, `seq(from, to, by)`, `rep(x, times)`, `x %in% set`.
