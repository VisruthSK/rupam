# Performance

Rupam's interactive and standalone execution paths share the language semantics in [Guarantees](guarantees.md). Optimisation must preserve evaluation scope, missingness, order, exact integers, diagnostics and effect boundaries.

## Cost of an interaction

- Completion and type inspection use the checked prefix and its schema. Opening a menu, moving the cursor or inspecting settings must never evaluate an expression or scan a dataset.
- Each submitted binding has an identity. Evaluate it once per valid revision. Reuse unchanged dependencies when compiling a draft. Cache evidence against its exact inputs, stage and environment, and invalidate only affected entries.
- Keep public table results columnar across process boundaries. Reuse Arrow buffers when their representation and lifetime permit it. Account for conversion when a host has a different representation. Fetch visible batches and columns, format cells at paint time, and release mappings when their last consumer closes.
- Scrolling a completed result performs no analysis. Run expensive compilation, scans and sorts outside the terminal event loop. Coalesce live edits, cancel superseded work and reject stale replies before publication. Preserve input focus and the last good grid throughout.
- Lower ordinary table operations to native vectorised expressions. Share repeated pure computations where scope and evaluation order permit. Discharge checks once at the earliest sound boundary. Retain runtime checks whose obligations depend on effects or unstable reductions.

## Implementation evidence

Exercise a cold session, a warm edit, repeated inspection, a wide table, a deep scroll and cancellation during a grouped computation. Record work at each boundary: checker invalidations, worker submissions, input scans, Arrow bytes copied, retained mappings, allocations and formatted cells. Compare full typed results alongside these observations. Separate host execution, compiler time, transport and terminal paint so an improvement in one cannot conceal a regression in another.

## OxCaml and Flambda 2

First compare an unchanged native compiler built with the stock OCaml toolchain and a compatible OxCaml toolchain using Flambda 2. Record compiler revision, optimisation flags, dependency compatibility, build time, executable size, allocations and profiles on identical workloads. Speedups in Rupam's OCaml stages must be measured separately from time in R, DuckDB and terminal I/O. Keep normal builds and conformance available on supported platforms during evaluation. [OxCaml compiler documentation](https://oxcaml.org/documentation/)

If profiles identify boxing in an OCaml loop, try unboxed numbers or products in that loop. For short-lived parser or layout scratch space, investigate local allocation. Preserve explicit ownership of Arrow buffers across the FFI and across worker lifetimes. Adopt representation changes only with a demonstrated benefit and a tested toolchain on the affected platforms. [Unboxed types](https://oxcaml.org/documentation/unboxed-types/intro/) · [Local allocation](https://oxcaml.org/documentation/stack-allocation/intro/)
