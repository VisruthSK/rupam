# Dialects

rupam compiles a checked program to a standalone script in a dialect: a host
language plus a table library.

| Dialect | Host | Library |
| --- | --- | --- |
| `r-dplyr` | R | dplyr, tidyr, bit64 |
| `python-polars` | Python | polars |

data.table, R polars and Julia are planned as further dialects.

## Pipeline

```text
source
  -> syntax tree with spans (recovers from errors)
  -> names resolved
  -> types, shapes and lifting inferred
  -> typed core: functions, values and relational verbs
  -> dialect lowering to a host-language syntax tree
  -> printed script, source map and manifest
```

The reference evaluator interprets the typed core. It is the test oracle that
every dialect must agree with; users never see its output.

## Dialect modules

Each dialect is one module that provides:

- a lowering for every core expression and every verb, producing a host
  syntax tree, never concatenated strings;
- a capability table saying, per construct, whether the dialect supports it
  exactly or not at all;
- the adapters it needs (below), its package list and how it reads and writes
  each file format.

Printing is shared by every dialect of a host language, so a new R dialect
reuses the R printer and source mapping. Changing how one verb lowers on one
dialect is a local edit. Source maps come from spans carried in the host
syntax tree. The manifest is computed from the typed core and the capability
table.

## Native code only

Every strict program lowers to the target's native vectorised operations.
rupam functions are pure expressions, so `if` and `match` become `case_when`
or `pl.when` chains and user functions are inlined into the expression.
There are no per-row callbacks, no `map_elements` and no interpreted loops.
A construct a dialect cannot express natively is unsupported on that dialect,
and compiling to it fails with a diagnostic naming the construct.

Generated code keeps pipelines as pipelines: one dplyr chain or one Polars
lazy query per table, with no temporary variables and no copies the library
would not make itself. Sequential `mutate` bindings become successive
`with_columns` calls only where Polars requires it.

## Adapters

A generated script contains the analysis and nothing else, except where a
target library's behaviour contradicts rupam's:

- reductions over nothing: R gives `NaN` for `mean` and `Inf` for `min`,
  where rupam gives `None`;
- `missing = propagate` on Polars reductions, which skip nulls by default;
- undefined floating-point results (`0 / 0`), which become runtime errors so
  that no NaN ever becomes a rupam value;
- printing values in rupam's format, so a script's output matches the REPL;
- the contract validation preflight, unless turned off.

Adapters are emitted only when the program uses the construct that needs
them.

## Release builds

`rupam compile --release` removes debug assertions: checks that only detect
programmer error, such as `pivot_wider` keys outside `levels` and index bounds
on vectors. A release script can differ from a debug run only where the debug
run would have stopped with an error. Adapters stay in release builds, because
without them the answers would differ.

## Strict mode

Strict mode is the default. A strict program contains only portable rupam:
no foreign imports and nothing whose meaning depends on the dialect. A strict
program previewed in one dialect and compiled to another gives the same
result, up to floating-point summation order, or fails to compile for a
dialect that cannot express it. `strict = false` in `rupam.toml` allows typed
foreign imports and target-specific functions; each one is recorded in the
manifest and limits the program to the dialects that provide it.

## The generated script

A generated script is an ordinary file that runs without rupam:

- readable, deterministic and formatter-friendly;
- with its contract preflight first, when there is one;
- with a source map from generated lines to `.rp` spans and a JSON manifest
  listing dialect, packages, validation steps and any non-portable calls;
- free to edit afterwards, though edits fall outside rupam's guarantees.
