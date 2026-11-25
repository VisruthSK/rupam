# Roadmap

The design lives in [the specification](spec/README.md). This page orders the
work that gets the implementation there. rupam is alpha: breaking changes that
improve ergonomics or performance are made freely.

## Order of work

1. `Int64` through `bit64` on R and checked through `Int128` in Polars;
   overflow and nonfinite results fail as [Numbers](spec/types.md#numbers)
   requires, through native checks where the host has them and narrow
   adapters where it does not; no `gmp` and no row callbacks.
2. Front end: the new syntax (`fn`, `if (c) a else b`, `TRUE`/`FALSE`, `_`,
   destructuring and punning, richer patterns, blocks, interpolation,
   prefixed literals, `^`, `%`, `%in%`, `~=`, `use` settings), Menhir
   incremental parsing for recovery, and spans on every node.
3. Types: vectors and shapes, lifting, rows, variants in columns, bare column
   references with ambiguity errors, built-in constraints, automatic optional
   injection, the missing-value policy, sign refinements, finite `Float64`,
   `ratio`, float equality, IEEE rounding.
4. One typed core with the reference evaluator as test oracle; dialect
   modules that build host syntax trees; native lowering only; adapters;
   rupam-format display; release builds; R-dplyr and Python-polars on it.
5. The REPL as an incremental compiler: warm workers, cached bindings,
   validation cached per file version.
6. Diagnostics with codes, labelled spans, fixes and `rupam explain`.
7. Newtypes and table keys.
8. Dates, times, date-times with zones, durations and periods.
9. Inline tables; `test` blocks and `validate` (both experimental); the
   conformance cases.
10. The REPL interface: highlighted prompt, completion menu, tibble-style
    printing, the full-screen viewer, and Windows CI that runs them.
11. Reading files without a contract: inline schemas, inferred schemas through
    DuckDB, `rupam schema`; the standard library (transforms, strings, regex,
    dates).

Everything in [Future](spec/future.md) comes after these.

## Formalisation

A Lean 4 development of the core calculus, kept in `formal/` and checked in
CI, alongside the implementation rather than after it:

- syntax and typing rules of the typed core: shapes, optional values, sign
  refinements and contextual requirements (coeffects) through higher-order
  calls;
- the guarded-evaluation rule shared by `if`, `match`, `let ... else`,
  `coalesce`, `and` and `or`, including reductions over whole groups;
- the missing-value policy and grouping witnesses;
- type soundness (progress and preservation) for the reference evaluator,
  and the theorem that sign refinements make undefined arithmetic
  unreachable.

The reference evaluator is the executable counterpart; conformance fixtures
check that it agrees with the formal rules on concrete programs.
