# Roadmap

An analyst opens a dataset, explores it at the prompt and in the viewer, and
keeps the useful steps as a readable `.rp` program that compiles to R/dplyr
or Python/Polars. The [specification](spec/README.md) defines semantics.
Every client consumes one [session](spec/session.md) through one path; the
developer MCP (`scripts/dev mcp`) is maintained alongside as infrastructure.

## 1. Semantic foundations

- Complete the type and TransformIR representations, including nominal and
  temporal columns and interval refinements.
- Define the calculus λR in [theory](theory/README.md): judgments for lifting,
  optionality, contextual reductions, guarded evaluation and observation
  domains, before implementing their combinations in both backends. The reference evaluator is the
  executable form; conformance fixtures pin each rule.
- Differential tests: generated well-typed programs run through the
  reference evaluator, R/dplyr and Python/Polars, comparing values, types,
  order and failures.

## 2. Reliable analytical core

- Ingestion of CSV, TSV and Parquet with exact `Int`, discovered schemas,
  parsing, dates and missing values.
- Evidence and locking: snapshots pinned at ingestion, evidence cached per
  input and stage, fused validation scans, incremental `:lock`, `:unlock`,
  `:refresh`.
- Table operations by family: selection, mutation, filtering, grouping,
  joins, binding, pivots, ordered windows and the standard library.
- Backend interfaces: each target satisfies a shared `BACKEND` signature
  assembled from a `HOST` and a `DIALECT` by a functor over shared abstract
  AST types, then passes shared conformance for missingness, numerics,
  guarding, grouping, alignment, cardinality and order.

## 3. Interactive experience

- The REPL prompt is the only editor: multiline, live-checked, with the grid
  updating from the current draft.
- A fast viewer over retained Arrow results: Rupam owns the grid, Mosaic the
  application shell, Matrix directly where profiling warrants it.
- Completion, diagnostics at spans, recoverable drafts, undo and redo.
- Editor support: TextMate grammar and an LSP over the shared checker.

## 4. Public MVP

Native releases for Linux, macOS and Windows; documentation; `rupam env`
environment setup; realistic end-to-end examples on both targets.

## 5. Reusable analytical ecosystem

Modules, package export, test blocks, Lean 4 proofs of the
[metatheory](theory/metatheory.md) in `formal/`, added incrementally from
phase 1 and checked against the reference evaluator on shared fixtures, and expanded
relational operations from [Future](spec/future.md).

## 6. Additional interfaces

The analyst MCP on MCP 2026-07-28 ([Agents](spec/agent-interface.md)), the
[notebook](spec/notebook.md) as an interactive development environment over
the same session, and report integration.
