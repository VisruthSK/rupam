# Overview

A rupam program is checked against the schema of its data, previewed in a
REPL, and compiled to a standalone R or Python script for a locked analysis.

## Design

Correct analytical code should be the easiest code to write. Infer types,
shapes, domains, ordering and structural properties wherever possible.
Require an explicit decision only where different choices change analytical
meaning or a necessary property cannot be established. Handle edge cases
without imposing ceremony, warnings or redundant work on ordinary analysis.
Prefer small, composable, predictable rules and useful analytical diagnostics.
Generated code should be readable native code, not a defensive runtime.

## Rules

1. Types and constraints are inferred. Annotations are optional and checked.
2. Strict programs share portable semantics; numerical rounding, clock values
   and random streams have the documented limits in [Dialects](dialects.md).
3. The REPL runs compiled code. A locked preview and script agree for the
   same dialect, effect inputs and environment.
4. Values are immutable. Functions are pure; top-level effects execute in source order. A live viewer draft runs automatically only when its evaluation is pure.
5. Static and locked-analysis proofs emit no redundant checks. Complete pure
   snapshots can establish schemas and concrete domain facts; explicit locking
   commits them. Validate data
   and dependencies at the boundary and emit only necessary native checks or
   adapters. Changed inputs require reanalysis and recompilation.
6. Prefer native vectorised table operations and readable pipelines; no
   per-row callbacks for core table computations.
7. Name competing analytical strategies explicitly (for example, missing-value policies). Use booleans for genuine on/off settings such as strict checking.
8. The prompt, viewer and saved program use the same Rupam expressions, logical types and diagnostics. Inspection reads existing evidence. Evaluation that can change external state requires explicit submission.

## Out of scope

Statistical modelling and graphics (deferred, see [Future](future.md)), SQL
targets, distributed execution, matrices, positional table indexing, notebook
formats, dimensional analysis, and inline R or Python.
