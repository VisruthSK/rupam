# Standard library

Functions lift per [Types](types.md). Table operations use native vectorised
lowerings or narrow adapters; the common supported core is available in both
dialects. Reductions take `missing =` for optional inputs. Ordinary vector
functions may use host higher-order operations. `T?` in the signatures below
is conservative: inference removes optionality when definedness is proved.

## Reductions

| Function | Result |
| --- | --- |
| `n()` | `Int64`, row count of the table or group |
| `n_distinct(x)` | `Int64`, or `Int64?` under `propagate` with optional input; number of distinct present values under `skip` |
| `sum(x)` | `Int64?` for integer or Boolean input, `Float64?` for float input; `None` over no present values |
| `prod(x)` | `Float64?`; `None` over no present values |
| `mean(x)`, `median(x)` | `Float64?`; `None` over no present values |
| `var(x)`, `sd(x)` | `Float64?`, sample statistics (divisor `n - 1`); `None` under two present values |
| `min(x)`, `max(x)` | `T?` for comparable `T`; `None` over no present values |
| `quantile(x, p, method = type7)` | `Float64?`; R's quantile conventions |
| `mad(x)`, `iqr(x)` | `Float64?`, robust spread |
| `any(x)`, `all(x)` | `Bool?` over Boolean input; see Missing values |
| `sum(b)`, `mean(b)` on `Bool` | count and proportion of `TRUE`, respectively |
| `first(x)`, `last(x)` | `T?`; total observation order or ordered vector required |

Numeric statistics use R conventions unless stated otherwise. Numeric
statistics accept numeric input; `sum` and `mean` also accept Bool. `min` and
`max` accept comparable input. Floating results use the numeric conversion rules.
Quantile `p` is a scalar in [0, 1]; `method = type1` through `type9` denote
R's nine quantile definitions, default type 7: interpolate the sorted values
at one-based position `1 + (n - 1) * p`. `iqr` is the type-7 upper quartile
minus the lower quartile. `mad` is `1.4826 * median(abs(x - median(x)))`;
`mad(x, constant = 1.0)` gives the unscaled MAD. Empty input is missing as
specified in [Missing values](missing.md), including for `sum` and `prod`.
Integer sums and counts use exact arithmetic and check the final result
against the portable range; no order-dependent wraparound or intermediate
range failure for a representable final integer sum. Use a wider/exact
accumulator or a proved-safe native reduction. For example `[MAX, 1, -MAX]`
sums to 1 in any order. Counts use the same final range requirement. For
Float64 reductions use numerically stable host algorithms where available;
if an intermediate overflow can be avoided by a semantics-preserving stable
algorithm, prefer it. Nonfinite final results are errors. Native floating
rounding and algorithm limitations remain documented; a backend may fail on
intermediate overflow rather than silently return a wrong result, and fixtures
must distinguish this limitation from integer exactness. `first` and `last`
under `skip` select the first or last present value; under `propagate` any
missing input makes the result missing. `n_distinct` under `propagate` also
returns `None` if any input is missing. Exact relational equivalence, not
approximate numerical comparison, defines distinctness.

## Transforms

Return an aligned column or vector of the input's length. Under `by =` they
apply per group. Numeric transforms use Float64 output except integer
cumulative sums and integer-valued ranks; undefined results are optional.

| Function | Result |
| --- | --- |
| `center(x)` | `x - mean(x)` |
| `scale(x)` | `x / sd(x)` |
| `standardize(x)` | `(x - mean(x)) / sd(x)`; `method = robust` uses median and scaled MAD |
| `rescale(x)` | `(x - min(x)) / (max(x) - min(x))`; `to = [a, b]` maps to the range |
| `rank(x, ties = average)` | one-based rank by ascending value, with `average`, `min`, `max` or `dense` ties |
| `percent_rank(x)` | `(rank(x, ties = min) - 1) / (present_count - 1)`; `None` for a singleton |
| `ntile(x, n)` | one-based integer bucket; sorted ascending by x, ties broken by total observation order |
| `cumsum(x)`, `cummean(x)` | ordered prefixes |
| `lag(x, n = 1, default = None)`, `lead(x, n = 1, default = None)` | shift by positions; boundary fill |

`scale`, `standardize` and `rescale` return `None` for every element when the
divisor is zero or undefined. `rescale` defaults to `(0.0, 1.0)` and permits
any finite endpoints. Average rank is Float64; min/max/dense ranks are
Int64. Dense ranks count distinct smaller values plus one. `ntile` requires a
positive Int64 bucket count; distribute observations into buckets differing
in size by at most one, larger buckets first. Distinct values establish the
rank order directly; only ties require meaningful observation order, not
arbitrary physical row positions. Positional transforms accept
operation-local `order_by =` (see [Tables](tables.md#order)).

For centering, scaling, ranking and rescaling, `skip` computes using present
observations, preserving missing positions; `propagate` produces missing
results for the entire affected group. For cumulative operations, `skip`
returns `None` at missing positions and continues the accumulator; `propagate`
returns `None` at and after the first missing value. `error` fails on missing
input. `cumsum` of Int64 is checked Int64. `lag`/`lead` preserve missing
positions and shift by a nonnegative Int64 offset; they take no missing
policy because they do not drop or aggregate values. A compatible `default`
fills only positions outside the input. Output is optional if a missing
input or boundary can occur.

## Numbers

`abs`, `sign`, `sqrt`, `exp`, `log`, `log2`, `log10`, `floor`, `ceiling`,
`trunc`, `round(x, digits = 0, ties = even)`, `pmin(a, b)`, `pmax(a, b)`,
`clamp(x, lo, hi)`, `div`, `ratio`, `positive`, `nonnegative`, `nonzero`,
`is_approx(a, b, atol = 0.0, rtol = sqrt(eps(Float64)))`.

`abs` preserves numeric type; `sign` returns Int64 (-1, 0, 1); transcendental
and rounding functions return Float64. `div` returns Int64. `pmin`, `pmax`
and `clamp` use a compatible common comparable type; `clamp` requires
`lo <= hi`. `min` and `max` reduce; `pmin` and `pmax` are element-wise.
Numeric domains, promotion, rounding and finite-result rules are in
[Types](types.md#numbers). `ratio` returns Float64?; sign helpers return the
refined numeric type or `None`. Safe helpers handle their documented domain
failures, not arithmetic overflow.

## Parsing

`parse_int`, `parse_float`, `parse_bool`, `parse_date(s, format)`,
`parse_datetime(s, format, zone)` and `parse_enum(s, enum = E)` return `T?`:
`None` where text does not parse or a value cannot be represented. Numeric
parsers consume the whole string, respect the portable range and finite-float
rules, and use locale-independent decimal notation. Boolean text is `TRUE`
or `FALSE`. Enum parsing matches the declared stored value exactly (parse numeric or
Boolean stored values with the corresponding primitive parser first). Date
and time parsers follow the formats and zone rules below; ambiguous or
nonexistent local times require the same explicit strategies as construction.

## Missing values

`coalesce(x, fallback)` selects the first present value, using compatible
common types and returning a present result when the fallback is present.
The fallback is evaluated only where needed. `some(x)` and `none(x)` return
present Bool, including for already nonoptional inputs, and refine presence.

## Strings

`str_length`, `str_to_lower`, `str_to_upper`, `str_trim`, `str_pad`,
`str_sub`, `str_starts`, `str_ends`, `str_detect`, `str_extract`,
`str_extract_all`, `str_replace`, `str_replace_all`, `str_split`.
Concatenation is interpolation. Text is valid UTF-8; lengths, substring
indices and padding count Unicode code points (one-based inclusive
substrings), not bytes or grapheme clusters. Case and whitespace operations
use the locked Unicode version, independent of locale. No implicit Unicode
normalisation. Search returns Bool; extraction returns String? or [String]
for all matches; splitting returns [String], including empty fields.
Replacement and regex matches use the common regex semantics below. String
ordering is UTF-8 byte order.

## Regex

Literal characters, `.`, character classes and ranges, `\d \w \s` and
negations, `^ $`, groups, non-capturing groups, named groups, alternation,
and `* + ? {m,n}` with lazy forms. No backreferences or lookaround. Patterns
are validated during checking. Match leftmost-first, nonoverlapping; dot
excludes newline, anchors match the string boundaries, and shorthand classes
use ASCII digit/word/whitespace definitions in both dialects. Unicode text
is otherwise matched by code point. Unmatched extraction is `None`, not an
empty string; all-match functions return empty vectors when no match exists.
A dialect without a matching lowering rejects the construct rather than
inheriting conflicting regex semantics.

## Dates and times

`today()`, `now()` (top-level effects), `year`, `month`, `day`, `wday`, `yday`,
`hour`, `minute`, `second`, `floor_date(x, unit)`, `to_zone`, `assume_zone`,
`days`, `hours`, `minutes`, `seconds` (durations), `months`, `years`
(periods), `between(x, lo, hi)` (inclusive bounds).
Use the proleptic Gregorian calendar, Monday = 1 for `wday`, and one-based
month/day/year-day. Calendar operations and formatting use the locked zone
database; `to_zone` preserves the instant. `floor_date` rounds down to the
start of the given calendar/time unit in the value's zone. Unsupported date
ranges or timestamp precision are capability errors, not silent truncation.
See [Types](types.md#dates-and-times) for arithmetic and DST rules.

## Vectors

`length(xs)` returns Int64; `rev`, `sort` and `unique` return vectors of the
input element type (`unique` retains first occurrences); `sort` uses the
common ordering with missing last. `map(xs, f)` maps a pure unary function;
`keep(xs, f)` retains elements whose predicate is true, requiring a missing
policy for optional predicates; `reduce(xs, init, f)` folds left in vector
order, returning `init` on empty input; each accumulator has the inferred
common type of `init` and `f`'s result. `seq(from, to, by)` is inclusive of
reachable endpoints, has nonzero step and compatible numeric inputs;
inconsistent step direction yields an empty vector. `rep(x, times)` repeats
values for a nonnegative Int64 count. `x %in% set` uses exact compatible
relational equivalence on present values; missing operands follow ordinary
optional comparison semantics (a present match determines TRUE, otherwise
unknown if a candidate is missing). `assert_key`, `desc`, `across`, selectors,
`Table`, `expand` and `complete` are contextual operations defined in
[Tables](tables.md). Seeding and random effects are in [Dialects](dialects.md).
