# Types

## Inference

Every binding's type is inferred: Hindley–Milner with row polymorphism,
built-in constraints, shapes and optional values. Annotations are allowed
anywhere and are checked. A function generalises over everything it does not
use, so

```text
let station(note) = note.station
```

has type `({station: a, ..r}) -> a` and accepts any record or table row with a
`station` field.

## Value types

| Type | Values |
| --- | --- |
| `Bool` | `TRUE`, `FALSE` |
| `Int64` | exact 64-bit integers |
| `Float64` | finite IEEE doubles and ±infinity; never NaN |
| `String` | UTF-8 text |
| `Date`, `Time`, `DateTime<Z>`, `Duration`, `Period` | see [Dates and times](#dates-and-times) |
| `Regex` | compiled patterns in rupam's regex dialect |
| `T?` | a `T` or `None` |
| `[T]` | vectors |
| `(A, B)` | tuples |
| `{a: A, b: B}` | records |
| `A -> B` | functions |
| `Table<{...}>` | tables (see [Tables](tables.md)) |
| user types | aliases, newtypes, variants |

There is no null, NA or NaN. Missing data is `None` and nothing else.

## Built-in constraints

Type variables can carry one of a fixed set of constraints, in the style of
Elm. There are no user-defined type classes.

| Constraint | Satisfied by |
| --- | --- |
| `number` | `Int64`, `Float64`, numeric newtypes that derive it |
| `comparable` | numbers, `String`, `Bool`, dates and times, enums (in declaration order), tuples and vectors of comparables |
| `equatable` | everything except `Float64`, functions and types containing them |
| `showable` | everything except functions |

`mean` works on any `number`; `==` works on strings, enums and records; `<`
works on dates.

## Vectors, shapes and lifting

A vector `[T]` is the everyday collection. A column of a table is a vector
tied to that table's rows. Every expression has a shape: a single value, a
standalone vector, a column of one particular table, or a table.

Calling a function on vectors or columns lifts it:

- A function whose body only uses single values is applied element by
  element. `let square(x) = x * x` gives `square(3) == 9` and
  `square(revenue)` is a column.
- A function that calls a reduction (`mean`, `sd`, `sum`, ...) sees the whole
  vector, or each group under `by =`. `let standardize(x) = (x - mean(x)) / sd(x)`
  applied to a column standardises that column.
- Single values broadcast to every element. Vectors never recycle: combining
  vectors of different lengths is an error, reported statically when the
  lengths are known and at run time otherwise.
- Columns of two different tables cannot be combined. Relate them with a join.

Lifting is decided by the types, recorded in the compiled program, and shown
on hover. It never depends on what a target library happens to accept.

## Optional values

`T?` holds a `T` or `None`. A plain `T` is accepted wherever a `T?` is
expected, so `if (v > 0.0) log(v) else None` needs no `Some`. A `T?` is never
accepted where a `T` is expected.

Operations on a missing value produce a missing value: `None + 1`,
`None == None`, `TRUE or None` and `f(None)` for an element-wise `f` are all
`None`. Only three things turn a missing value into a present one: the
missing-value policy at a decision point ([Missing values](missing.md)),
`coalesce`, and `match`.

`if` and `filter` need a `Bool`, so a `Bool?` condition is a type error until
the missing case is handled.

## Numbers

- `Int64` is exact across its range in every dialect. Results outside the
  range are unspecified. `prod` of integers returns `Float64`.
- `Float64` follows IEEE 754 except that NaN does not exist. Dividing a
  nonzero number by zero gives ±infinity. An operation with no defined result
  (`0 / 0`, `Inf - Inf`, `0 * Inf`, `sqrt` or `log` of a negative number) is a
  runtime error naming the expression and the first failing row. `ratio(a, b)`
  returns `Float64?`, with `None` where `a / b` is undefined. NaN in an input
  file is read as `None`.
- `/` always returns `Float64`. `div(a, b)` is integer division and `%` is
  modulo; both follow the sign of the divisor, as in R and Python.
- There are no implicit conversions between `Int64` and `Float64` except for
  literals that are exactly representable. `float(n)` and `int(x)` convert
  explicitly; `int` needs a whole number.
- `Float64` is not `equatable`. `==` on floats is a type error. `a ~= b` (also
  written `a ≈ b`) is approximate equality: relative tolerance
  `sqrt(eps)` (about 1.5e-8), switching to absolute difference when the
  reference value is near zero, as R's `all.equal` does. A file that starts
  with `use float_equality = approximate` gives `==` that meaning on floats.
- `round(x, digits)` rounds half to even, the IEEE 754 default and what R and
  Python do. `round(x, digits, ties = away)` rounds half away from zero. Every
  dialect rounds identically.
- Decimal arithmetic is deferred; see [Future](future.md).

## Records and rows

Records are immutable and structurally typed. Row types are open:
`{revenue: Float64, ..r}` means "has `revenue`, and whatever else". Table rows
are records, so every function over records is a function over tables.

## Variants and enums

```text
type Quality = | Absent | Invalid | Zero | Valid
type Reading = | Measured(value: Float64) | Offline | Fault(reason: String)
```

A variant with no fields everywhere is an enum. Enums order by declaration,
which is how ordered categories work; there are no factors. Variants can be
stored in columns: enums lower to the target's categorical type, variants with
fields to struct columns.

### Exhaustiveness

The checker decides exhaustiveness with Maranget's usefulness algorithm over
the pattern matrix (Maranget, *Warnings for pattern matching*, 2007).
`Bool`, `T?`, tuples, vectors (by length) and declared variants have finite
heads; numbers and strings do not, so a match on them needs a final catch-all.
The error names one missing value, such as `Missing case Measured(None)`.

## Newtypes

```text
type PatientId = new Int64
type Usd = new Float64 deriving (number, comparable)
```

A newtype is a distinct type with the same representation as its base and no
run-time cost. `PatientId(5)` wraps and `let PatientId(n) = id` unwraps. A
newtype has none of its base type's operations unless it derives them:
`PatientId` supports `==` (every newtype of an `equatable` type is
`equatable`) but not `+`, so adding two ids is a type error, and a join of
`PatientId` against `HospitalId` does not type-check.

## Dates and times

| Type | Meaning |
| --- | --- |
| `Date` | a calendar date |
| `Time` | a wall-clock time of day |
| `DateTime<Z>` | an instant, displayed in zone `Z` (`DateTime<"UTC">`) |
| `Duration` | an exact length of time |
| `Period` | a calendar amount: days, months, years |

- Zones are part of the type. Comparing or combining date-times in different
  zones is a type error until one is converted with `to_zone`.
- There are no naive date-times. Reading one requires its zone, from the
  contract, the inline schema or `assume_zone`.
- `Date` and `DateTime` never compare directly.
- `DateTime + Duration` is exact. `Date + Period` and `DateTime + Period` are
  calendar arithmetic: adding months clamps to the end of the month, so
  `d"2026-01-31" + months(1) == d"2026-02-28"`.
- Building a date-time from a local wall-clock reading that falls in a
  daylight-saving gap or overlap requires an explicit choice
  (`nonexistent = shift_forward`, `ambiguous = earliest`).

## Effects

Functions are pure. Reading files, writing files, the clock and randomness are
effects, allowed only at the top level of a program. A pure function is
never run once per row through a callback.

## Immutability and shadowing

Nothing mutates. `let` is the only way to name a value, and a name never
changes what it refers to; `let` is therefore also how constants are written.
A later `let` with the same name shadows the earlier one:

```text
let orders = load(shop.orders)
let orders = orders |> filter(revenue > 0.0)
```
