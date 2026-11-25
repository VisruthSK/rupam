# The language

rupam infers every type, never mutates a value, and writes every call the same
way: `f(a, b, name = c)`. This page covers the core language. Tables are in
[tables.md](tables.md).

## Values

```text
42   1_000_000      # Int64
3.5  1e-3           # Float64
"Ridge"             # String
TRUE  FALSE         # Bool
None                # a missing value of any type
(1, "a")            # tuple
[1, 2, 3]           # List<Int64>
{station = "Marsh", altitude = 1210}   # record
```

`/` always gives a `Float64`. An integer literal becomes a `Float64` when it
meets one, so `x > 10` works for a decimal `x`, but only when the value is
exactly representable. Otherwise `Int64` and `Float64` never mix.

A value of type `T?` may be missing. Present values of type `T` are accepted
wherever a `T?` is expected, so there is no `Some` to write. Arithmetic,
comparisons and function calls on a `T?` give a `T?` that is missing where the
input is:

```text
let cost: Float64? = None
cost * 2.0                 # None : Float64?
coalesce(cost, 0.0)        # 0.0 : Float64
```

## Bindings and functions

```text
let threshold = 2.0
let square(x) = x * x
let clamp(x, lo, hi) = if (x < lo) lo else if (x > hi) hi else x
let twice = fn(f, x) => f(f(x))
let rounded = round(_)          # a function of the hole

let area = {
  let r = 2.0
  3.14159 * square(r)
}
```

A top-level item ends at the end of a line once it is complete, or at `;`. A
line that ends in an operator, `=`, `=>`, `,` or an open bracket continues, and
so does a line followed by one starting with `|>`, `else` or a closing bracket.

`x |> f(a)` is `f(x, a)`, and `x |> f(a, _)` puts `x` in the hole.

`{site, n}` is short for `{site = site, n = n}`. Backticks quote any name:
`` `price per unit` ``.

## Annotations

They are optional and checked when present.

```text
let shift(δ: Float64): Float64 = δ + 1.0
let label(r: {name: String, ..rest}) = r.name
```

Types are written `Int64`, `Float64`, `String`, `Bool`, `T?`, `List<T>`,
`(A, B)`, `A -> B` and records `{a: A, ..row}`.

## Records

A function that reads `r.station` accepts any record with a `station` field:

```text
let station(note) = note.station
station({station = "Ridge", altitude = 1210})
station({station = "Marsh", flooded = TRUE})
```

## Variants

```text
type Reading<a> =
  | Measured(value: a)
  | Offline
  | Fault(reason: String)
```

Constructors start with an uppercase letter and are called like functions:
`Measured(0.5)`, `Fault("calibration expired")`.

## Pattern matching

```text
let group(age) = match age {
  ..<18 => "child",
  18..<65 => "adult",
  65.. => "senior",
}

let assess(reading) = match reading {
  Measured(value) if value < 0.0 => "below baseline",
  Measured(_) => "usable",
  Offline | Fault(_) => "unavailable",
}
```

A pattern can be `_`, a name, a literal, a range (`a..=b`, `a..<b`, `..<b`,
`a..`), a constructor, `Some(p)` or `None`, a tuple, a record `{a, b}`, a set
of alternatives `p | q`, or `name @ p`, which binds the whole value. An arm
can end in a guard, `if condition`. `match a, b { p, q => ... }` matches two
values at once.

Every `match` must be exhaustive, and the error names a case you missed:

```text
error: 3:3: Non-exhaustive match. Missing case Measured(None)
```

Ranges that cover every number count as exhaustive, as in `group` above.
Guarded arms don't count.

## Operators

From tightest to loosest: calls and field access, `^`, unary `-`, `* / %`,
`+ -`, comparisons and `%in%`, `not`, `and`, `or`, `|>`. Comparisons don't
chain, and `-x^y` is an error: write `-(x^y)` or `(-x)^y`.

```text
region %in% ["North", "South"]
```

## Standard library

Numbers: `abs`, `sign`, `sqrt`, `exp`, `log`, `log2`, `log10`, `floor`,
`ceiling`, `trunc`, `round`, `pmin`, `pmax`. Missing values: `coalesce`,
`some`, `none`. Text: `str_length`, `str_to_lower`, `str_to_upper`,
`str_trim`, `str_starts`, `str_ends`, `str_detect`. Inside table verbs you
also get the reductions `sum`, `mean`, `median`, `var`, `sd`, `min`, `max`
and `n_distinct`.

## Running programs

```sh
rupam run --target python-polars examples/field-notes.rp
```

`rupam run` compiles the program and runs the generated script, which prints
the value of every top-level expression. `rupam compile` writes the script
instead. [examples/language-tour.rp](../examples/language-tour.rp) holds every
example on this page.
