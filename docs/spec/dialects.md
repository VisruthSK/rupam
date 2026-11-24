# Dialects

A dialect is a host language and table library.

| Dialect | Host | Libraries |
| --- | --- | --- |
| `r-dplyr` | R | dplyr, tidyr, bit64 |
| `python-polars` | Python | polars |

## Compilation

```text
source -> syntax tree with spans -> resolved names -> inferred types, shapes and refinements
       -> typed core -> host syntax tree -> script, source map, manifest
```

The reference evaluator interprets the typed core and defines expected results
for [conformance](conformance.md). It is not used to run programs.

## Dialect modules

A dialect provides:

- a lowering of every core expression and verb to a host syntax tree;
- a capability table listing the constructs it supports;
- its adapters, packages, and readers and writers per file format.

Host printing and source mapping are shared by dialects of the same language.
The manifest is derived from the typed core and the capability table.

## Lowering

- Every program lowers to native vectorised operations. `if` and `match`
  lower to conditional expressions; user functions are inlined. No per-row
  callbacks.
- A construct without a native lowering in a dialect is unsupported there;
  compiling it is a type error naming the construct.
- A table expression lowers to one pipeline: one dplyr chain, one Polars lazy
  query. No temporary variables.

## Adapters

A generated script contains only the analysis and these adapters, each
emitted only when used:

| Adapter | Dialect |
| --- | --- |
| reductions over no values return `None` (`mean` gives `NaN`, `min` gives `Inf` natively) | R |
| `missing = propagate` on reductions (nulls are skipped natively) | Polars |
| NaN in floating-point input becomes `None`; infinite input is rejected | both |
| values print in rupam format | both |
| contract validation before reading contracted sources | both |

## Release builds

`rupam compile --release` omits debug assertions: vector bounds and
`pivot_wider` levels. A release script differs from a debug run only where
the debug run fails.

## Strict mode

Strict mode is the default. A strict program contains no foreign imports and
no dialect-specific functions, and produces the same result in every dialect
up to floating-point summation order. With `strict = false`, foreign imports
and dialect-specific functions are allowed, recorded in the manifest, and
restrict the program to the dialects that provide them.

## Generated scripts

- Run without rupam.
- Deterministic and formatter-friendly.
- Accompanied by a source map to `.rp` spans and a JSON manifest of dialect,
  packages, validation steps and non-portable calls.
