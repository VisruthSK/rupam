# Roadmap

An analyst opens a dataset, explores it at the prompt and in the viewer, and
keeps the useful steps as a readable `.rp` program that compiles to R/dplyr
or Python/Polars. The [specification](spec/README.md) defines semantics and
[conformance](spec/conformance.md) pins them; this page lists only what is
not yet built.

## 1. Semantic foundations

- Generate differential programs from typed TransformIR rather than source
  text, and shrink each failure into a pending fixture.
- `DateTime`, `Time` and zones; nominal types and units (C33, C34, C95).

## 2. Analytical core

In order of conformance cases unlocked:

1. Contextual helpers passed as values and nested contextual reductions
   (C60, C62, C124), multi-column `match` in verbs (C127), quoted output
   names (C103), `Int()` conversion (C40, C49, C64).
2. A fixture runner for the session protocol: drafts, holes, decisions,
   `define`, `fork`, `save`, target switches and taps (about 11 cases).
3. Contract validation: required, range, NaN and infinity in sources, keys
   and foreign keys, malformed fields (about 9 cases).
4. Evidence and locking: incremental `:lock`, `:unlock`, `:refresh`, schema
   discovery accepted by lock, and obligations discharged by locked evidence
   (about 13 cases).
5. Reshaping: `bind_rows`, `complete`, `expand`, pivot duplicates and
   `values_fn` (about 7 cases).
6. `across` and the remaining column transforms (C77, C106).
7. Ordered windows: `lag`, `lead`, `slice_min` with total-order obligations
   (5 cases).
8. Strict mode and effect sets (5 cases); restricted columns (3 cases).

## 3. Interactive experience

- `:probe`, `??term` search, comment-preserving `:save`.
- A TextMate grammar; LSP completion and hover from the shared checker.
- Frame-time measurements of the viewer on large results.

## 4. Public MVP

Native releases for Linux, macOS and Windows; documentation; `rupam env`
environment setup; realistic end-to-end examples on both targets.

The last step before release is a whole-codebase audit: every algorithm is
log-linear at worst with small constants, accidental complexity and spec
deviations are removed, systems are simplified, hand-written infrastructure
moves to maintained dependencies where one fits, and tests stay native OCaml,
with R and Python only as reference backends.

## 5. After the MVP

- Data-discovered enums and `as_enum` ([Future](spec/future.md)).
- Splitting each backend into a `HOST` and a `DIALECT` once a language has a
  second table library.
- Modules, package export, test blocks, `repeat`, and the relational
  extensions in [Future](spec/future.md).
- Lean 4 proofs of the [metatheory](theory/metatheory.md) in `formal/`,
  checked against the reference evaluator on shared fixtures.
- The analyst MCP on MCP 2026-07-28 ([Agents](spec/agent-interface.md)) and
  the [notebook](spec/notebook.md) over the same session.
