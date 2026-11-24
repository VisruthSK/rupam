# The language

rupam is an ML with a friendlier surface. Types are inferred, values are
immutable, and every function call is written the same way: `f(a, b, name = c)`.
This page covers the core language. Tables, which are what you actually come
here for, are in [tables.md](tables.md).

## Values

```text
42                  # Int64, overflow is an error, not a wraparound
3.5   1e-3          # Float64
"Ridge"             # String
true  false         # Bool
None  Some(3)       # Option<Int64>
(1, "a")            # tuple
[1, 2, 3]           # List<Int64>
{station = "Marsh", altitude = 1210}   # record
```

`/` always produces a `Float64`. `Int64` never turns into `Float64` behind your
back. A plain `None` is `Option<'a>` until something pins down `'a`.

## Bindings and functions

```text
let threshold = 2.0
let square(x) = x * x
let clamp(x, lo, hi) = if x < lo then lo else if x > hi then hi else x
let twice = fn(f, x) => f(f(x))

let area = let r = 2.0 in 3.14 * square(r)
```

Top-level statements end at a newline once the expression is complete, or at
`;`. A line that ends in `|>`, `=`, an operator or a comma continues onto the
next line, and so does a line followed by one that starts with `|>`, `then` or
`else`.

`x |> f(a)` means `f(x, a)` and `x |> f` means `f(x)`.

Names can contain Unicode letters. Backticks quote anything else:
`` `price per unit` ``.

## Annotations

You rarely need them, but they are checked when present.

```text
let shift(δ: Float64): Float64 = δ + 1.0
let first(p: (Int64, String)): Int64 = match p { (n, _) => n }
let label(r: {name: String, ..rest}) = r.name
```

The type syntax is `Int64`, `Float64`, `String`, `Bool`, `T?` (short for
`Option<T>`), `List<T>`, `(A, B)`, `A -> B` and records `{a: A, ..row}`.

## Records and row polymorphism

A function that reads `r.station` accepts any record that has a `station`
field, whatever else it carries:

```text
let station(note) = note.station
station({station = "Ridge", altitude = 1210})
station({station = "Marsh", flooded = true})
```

`:type station` in the REPL prints `({station: 'a0, ..'a1}) -> 'a0`.

## Types and variants

```text
type Pair<a> = {x: a, y: a}

type Reading<a> =
  | Measured(value: a)
  | Offline
  | Fault(reason: String)
```

The first form is an alias. The second, with a leading `|`, declares a variant.
Constructors start with an uppercase letter, take named fields and are called
like functions: `Measured(Some(0.5))`, `Fault("calibration expired")`.

## Pattern matching

```text
let assess(reading) =
  match reading {
    Measured(Some(value)) => if value < 0.0 then "below baseline" else "usable",
    Measured(None) => "missing value",
    Offline => "offline",
    Fault(reason) => reason
  }
```

Patterns are `_`, a name, a literal, a constructor with sub-patterns, `Some(p)`,
`None`, a tuple `(p, q)` and a record `{a, b}`, which binds those fields by name.
A name may appear only once in a pattern.

Every `match` must be exhaustive, and the checker gives you a missing case
rather than a yes/no:

```text
error: 3:3: Non-exhaustive match. Missing case Measured(None)
error: 1:15: Non-exhaustive match. Missing case (true, true)
```

It reasons about `Bool`, `Option`, tuples and your own variants, nested to any
depth. Integers and strings have too many values to enumerate, so a match on
them needs a final `_` or name, and the missing case is reported as `_`.
[The specification](spec/types.md#exhaustiveness)
gives the algorithm.

## Operators

From tightest to loosest: calls and field access, unary `-` and `not`,
`* /`, `+ -`, comparisons, `and`, `or`, `|>`. Comparisons do not chain;
write `a < b and b < c`. Arithmetic and comparison currently need numeric
operands on both sides, so `"a" == "b"` is a type error for now. `%` parses
but has no typing rule yet.

## Running programs

```sh
rupam run --target python-polars examples/field-notes.rp
```

`rupam run` compiles the program for the target dialect and runs the
generated script, which prints the last value. `rupam compile` writes that
script instead. The REPL type-checks and evaluates definitions as you type
them. Programs with a contract are covered in [tables.md](tables.md).
