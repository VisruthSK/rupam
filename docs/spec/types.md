# Types

## Inference

Types are inferred by Hindley–Milner inference extended with row
polymorphism, built-in constraints, shapes, optional values and sign
refinements. Every binding generalises over what it does not use:

```text
let station(note) = note.station      # ({station: a, ..r}) -> a
```

## Value types

| Type | Values |
| --- | --- |
| `Bool` | `TRUE`, `FALSE` |
| `Int64` | 64-bit integers |
| `Float64` | finite 64-bit floating-point numbers |
| `String` | UTF-8 text |
| `Date`, `Time`, `DateTime<Z>`, `Duration`, `Period` | [Dates and times](#dates-and-times) |
| `Regex` | patterns in the regex dialect ([Standard library](stdlib.md#regex)) |
| `T?` | a `T` or `None` |
| `[T]` | vectors |
| `(A, B)` | tuples |
| `{a: A, b: B}` | records |
| `A -> B` | functions |
| `Table<{...}>` | [tables](tables.md) |

There is no null, NA, NaN or infinity.

## Constraints

| Constraint | Types |
| --- | --- |
| `number` | `Int64`, `Float64`, newtypes deriving `number` |
| `comparable` | numbers, `String`, `Bool`, dates and times, enums (declaration order), tuples and vectors of comparables |
| `equatable` | all types except `Float64`, functions, and types containing them |
| `showable` | all types except functions |

There are no user-defined type classes.

## Shapes and lifting

Every expression has a shape: a single value, a vector, a column of a
particular table, or a table.

- A function whose body uses only single values applies element-wise to
  vectors and columns.
- A function that applies a reduction to a parameter receives the whole
  vector or column, or each group under `by =`.
- Single values broadcast. Vectors of different lengths do not combine: the
  error is static when lengths are known, otherwise at run time.
- Columns of different tables do not combine.
- The chosen lifting is part of the typed program and is shown on hover.

## Optional values

- `T?` is a `T` or `None`.
- A `T` is accepted where a `T?` is expected. A `T?` is not accepted where a
  `T` is expected.
- Every operation with a `None` operand returns `None`, including `==`,
  `and` and `or`.
- `None` becomes a value only through the missing-value policy
  ([Missing values](missing.md)), `coalesce`, or a pattern.
- `if` and `filter` require `Bool`.

## Numbers

- `Int64` arithmetic is exact within range. Results outside the range are
  unspecified.
- `Float64` arithmetic is IEEE 754 on finite values. Results that overflow
  are unspecified.
- `/` returns `Float64`. `div(a, b)` is integer division. `%` is modulo with
  the sign of the divisor.
- No implicit conversion between `Int64` and `Float64` except exactly
  representable literals. `float(n)` and `int(x)` convert; `int` requires a
  whole number.
- `==` is not defined on `Float64`. `a ~= b` (also `a ≈ b`) holds when
  `|a - b| <= sqrt(eps) * |b|`, or `|a - b| <= sqrt(eps)` when `|b| < sqrt(eps)`.
  In a file declaring `use float_equality = approximate`, `==` on `Float64`
  means `~=`.
- `round(x, digits)` rounds half to even. `round(x, digits, ties = away)`
  rounds half away from zero.
- `prod` of `Int64` returns `Float64`.

### Sign refinements

A numeric type may carry a sign refinement: `> 0`, `>= 0` or `!= 0`, written
after the type (`Float64 > 0`). Refinements are inferred and have no run-time
representation.

Requirements:

| Operation | Requires |
| --- | --- |
| `a / b`, `div(a, b)`, `a % b` | `b != 0` |
| `sqrt(x)` | `x >= 0` |
| `log(x)`, `log2(x)`, `log10(x)` | `x > 0` |
| `x ^ y` with `y : Float64` | `x >= 0` |

Sources:

- literals, by value;
- arithmetic on refined operands (sum of `>= 0` values is `>= 0`, and
  `> 0` if one is `> 0`; products follow sign rules; `abs`, even powers,
  `sqrt` give `>= 0`; `exp` gives `> 0`; `abs` of `!= 0` gives `> 0`);
- `n()` is `> 0` within a group and `>= 0` over a table;
- `filter(x > 0)`, `filter(x >= 0)` and `filter(x != 0)` refine `x` in later
  stages; the same comparisons in `if` conditions and match guards refine `x`
  in the guarded branch;
- validated contract assertions;
- `positive(x)`, `nonnegative(x)` and `nonzero(x)`, which return the refined
  value or `None`;
- `ratio(a, b)`, which is `a / b` when `b != 0` and `None` otherwise.

An operation whose requirement is not established is a type error.

## Records and rows

Records are immutable and structurally typed. A row type
`{revenue: Float64, ..r}` contains `revenue` and any other fields. Table rows
are records.

## Variants and enums

```text
type Quality = | Absent | Invalid | Zero | Valid
type Reading = | Measured(value: Float64) | Offline | Fault(reason: String)
```

- A variant whose constructors have no fields is an enum. Enums are ordered
  by declaration.
- Variants may be stored in columns.
- Exhaustiveness is decided by Maranget's usefulness algorithm. `Bool`,
  `T?`, tuples, vectors by length and declared variants have finite heads;
  other types require a catch-all. A missing case is reported as a concrete
  pattern.

## Newtypes

```text
type PatientId = new Int64
type Usd = new Float64 deriving (number, comparable)
```

- A newtype is distinct from its base type and from other newtypes, with the
  same representation.
- `PatientId(5)` constructs; `let PatientId(n) = id` deconstructs.
- A newtype is `equatable` if its base is. Other constraints require
  `deriving`.

## Dates and times

| Type | Meaning |
| --- | --- |
| `Date` | calendar date |
| `Time` | wall-clock time of day |
| `DateTime<Z>` | instant in time zone `Z` |
| `Duration` | exact length of time |
| `Period` | calendar amount in days, months and years |

- Date-times in different zones do not combine or compare. `to_zone`
  converts.
- Every date-time has a zone. A value read without one requires a zone from
  the contract, the inline schema or `assume_zone`.
- `Date` and `DateTime` do not compare.
- `DateTime + Duration` is exact. `+ Period` is calendar arithmetic; month
  arithmetic clamps to the last day of the month.
- Constructing a date-time from a wall-clock reading in a daylight-saving gap
  or overlap requires `nonexistent =` and `ambiguous =`.

## Effects

Functions are pure. Reading and writing files, the clock and randomness are
effects and appear only at top level.

## Bindings

Values are immutable. `let` binds a name; a later `let` with the same name
shadows it.
