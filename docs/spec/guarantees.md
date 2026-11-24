# Guarantees

A program that type-checks has these properties.

## Values

- Every value has one static type. Nothing converts implicitly.
- Vectors are homogeneous and never recycle.
- Missing data is `None` and nothing else. There is no NA, NULL, NaN or
  infinity.
- No arithmetic produces an undefined result: division, `sqrt`, `log` and
  fractional powers receive only values their sign refinements allow.
- `Float64` values are not compared with `==` except under an explicit
  `use float_equality = approximate`.
- `Int64` values are exact.
- Rounding is half to even unless `ties = away` is given.
- `var` and `sd` divide by `n - 1`.
- Values are immutable; closures capture values.
- Integer and floating-point literals are distinguished by syntax.

## Control flow

- `if` and `filter` receive exactly one `Bool` per row; never a missing
  value or a vector.
- Every `match` is exhaustive.
- `-x^y` and chained comparisons do not parse.

## Tables and columns

- Every column name used exists in the schema; names match exactly.
- A bare name never silently refers to both a column and a binding.
- Every decision point that receives an optional value has an explicit
  policy.
- Columns of unrelated tables never combine.
- A join states its keys, compares keys of identical type, and multiplies
  rows only under a stated `relationship`.
- Row order exists only after `arrange`. String order is byte order.
- Categories are enums; their values are checked.
- Ranges never count down. Indexes are present `Int64` values from 1.

## Dates and times

- Date-times in different zones never combine without `to_zone`.
- Every date-time has a zone.
- `Date` and `DateTime` never compare.
- Month arithmetic never yields an invalid date.

## Execution

- A preview and the compiled script of the same dialect produce the same
  result.
- Strict programs produce the same result in every supported dialect, up to
  floating-point summation order.
- Contracted sources are validated in full before any value is read.
