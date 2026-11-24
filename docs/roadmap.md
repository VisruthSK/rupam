# Roadmap

rupam is alpha. Any change that makes it nicer to write or faster to run goes
in, even when it breaks existing programs. There will be no editions.

## Settled design

- Vectors are the everyday value. Functions lift over vectors and columns
  automatically. Only single values broadcast; lengths never recycle. There are
  no linked lists.
- Vectors index from 1, with `x[end]`.
- Syntax borrows from R where R has a good spelling: `\(x) x + 1`,
  `if (c) a else b`, and `_` as the pipe placeholder. Local bindings use
  `let ... in`.
- Destructuring and punning work everywhere: `let (lo, hi) = r`,
  `let {name, age} = p`, `let [first, ..rest] = v`, `{x, y}`.
- Pattern matching gains ranges, or-patterns, guards, `@` bindings and
  `let ... else`.
- String interpolation is typed and complete. Prefixed literals are checked at
  compile time: `d"2026-11-24"`, `t"2026-11-24 09:00 America/Los_Angeles"`,
  `r"^\d+$"`, `raw"C:\data"`.
- `T?` is the only meaning of `?`: an optional value.
- Constraints are built in, Elm-style (number, comparable, equatable,
  showable). There are no user type classes.
- Newtypes are distinct and cost nothing at run time:
  `type PatientId = new Int64`.
- One missing-value policy, defined once, applies to every verb. A verb
  applied to an optional input must state the policy. Operations on a missing
  value give a missing value; only the policy, `coalesce` and `match` resolve
  one.
- `Float64` has no NaN. `Int64` is exact, with no overflow checks.
- `Date`, `DateTime` with time zones, and `Duration` are core types.
  Time-zone mistakes are type errors. Categories are ordinary enums.
- APIs take enums, never boolean flags.
- Static checks never appear in generated code. Debug-only assertions are
  removed by `compile --release`.
- An unused pipeline result is a warning.
- Errors carry a stable code, labelled source spans and a suggested fix.
- The REPL compiles each input and runs it in a warm R or Python worker, so a
  preview is the compiled program.

## Order of work

1. R `Int64` through `bit64`; remove overflow checks, row callbacks and `gmp`.
2. Front end: the new syntax, Menhir incremental parsing for recovery, and
   spans on every node.
3. Types: vectors and shapes, lifting, rows, ADTs in columns, bare column
   references, built-in constraints, automatic `Some`, the missing-value
   policy, and `ratio`.
4. One IR with the reference evaluator as test oracle; dialect modules that
   build host-language syntax trees; rupam-format display; debug and release
   profiles; R-dplyr and Python-polars ported onto it.
5. The REPL as an incremental compiler with warm workers, cached bindings and
   validation cached per data snapshot.
6. Diagnostics with codes, labelled spans, fixes and `rupam explain`.
7. Newtypes.
8. Dates, times and durations.
9. Inline tables and `test` blocks; `rupam test` on every dialect; the
   conformance fixtures.
10. The REPL interface: lambda-term input, pillar-style tables, a full-screen
    viewer, and Windows CI that runs them.
11. Loading files without a contract (`read_csv`, `read_parquet`) through
    DuckDB; strings, regex and statistics in the standard library.

## Later

- Typed foreign imports for R and Python.
- Exporting rupam functions as an R package or Python module that callers use
  without rupam.
- Model and graphics libraries on top of `~` formulas.
- More dialects: data.table, R polars, Julia.
- `rupam env` to create uv and renv environments.
- A language server built on linol.
