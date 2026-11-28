# Results that exploit what the compiler knows

**Status:** Staged across milestones; see the roadmap.

## Objective

Rupam knows things a dataframe library does not: every binding's schema and
dependencies, that results are immutable, the observation domain of each
table, its keys, and the evidence behind each obligation. Use that knowledge
to keep memory proportional to what is shown, keep inspection instant while
work runs, and avoid recomputation. No change to semantics: an unevaluated
computation never appears as an established result, and an optimisation
applies only where it preserves missingness, order, exact integers,
numerical behaviour and effects (`docs/spec/guarantees.md`).

Read first: `docs/spec/session.md`, `docs/spec/performance.md`,
`docs/briefs/compiler-platform-separation.md`,
`docs/briefs/interactive-performance.md`, `AGENTS.md`.

Every step is justified by a measurement on a representative session and
kept only if the measurement improves; none of these is a default to build
speculatively.

## 1. Inspection independent of execution (alpha.2)

A long join on the worker must not delay looking at a finished result.

- Completed results are read in-process from their published Arrow files, so
  viewport, summary and completion requests never queue behind the worker.
  Make this a tested guarantee: start a long computation, then page, scroll,
  complete and inspect types against earlier results within the latency
  budgets of the interactive performance brief.
- Scheduling classes, highest first: input and paint (one frame), cached
  viewport access, completion and type inspection, submitted analysis, then
  interruptible background work (full sorts, profiles, prefetch), then cache
  maintenance on idle resources. Background work runs under a memory budget
  and yields to foreground requests.
- Work that cannot be cancelled is isolated and its result discarded on
  arrival if superseded (`session.md` already rejects stale replies).

## 2. Demand-driven materialisation (0.1.0)

`clean = data |> filter(valid)`, `enriched = clean |> mutate(...)` and
`result = enriched |> summarize(...)` need not each hold a full copy.

- Keep three facts per binding separately in the session state machine:
  checked (type and obligations known), established (evaluated successfully
  for the current revision, with its evidence), and materialised (a physical
  result exists). Show only established results; a binding that is checked
  but not established is shown as such.
- Evaluate a binding when something demands it: the viewer, a preview, an
  export, a lock, or a dependent that cannot be fused into one backend query.
  Fuse chains of single-consumer bindings into one lazy backend query
  (Polars lazy frames, dplyr pipelines, SQL).
- Materialise a shared intermediate when more than one consumer demands it
  and recomputing it costs more than holding it; record that decision and its
  cost in the session, bounded by bytes.
- Evaluation must still happen automatically, as the session spec requires:
  laziness changes when and how a value is computed, never whether a checked
  binding is evaluated and its diagnostics surfaced.

## 3. Index views over immutable results (0.1.0)

- A sort or filter of a published result can be published as a permutation or
  a selection over the base result's buffers instead of a full copy: 32-bit
  row indices (4 MB for one million rows) or a selection bitmap. The backend
  still computes it (for example Polars `arg_sort`, a filter mask); the
  session keeps the base result alive while a view references it.
- Several viewer tabs can show different orderings of one result without
  duplicating columns; the online sort's top-k page is a view.
- Materialise a contiguous copy when a backend needs one or repeated access
  pays for it; indirection costs cache locality.
- Talon's result reader gains views; it does not become a dataframe engine.

## 4. After 0.1.0, each gated on a profile

- **Persistent content-addressed results.** Identify a pure result by the hash
  of its checked TransformIR, input snapshot digests, target and environment
  and semantic options; persist results and evidence in native storage (or
  OPFS in the browser). Never reuse results of effects, random draws or clock
  reads; backend and numeric semantics are part of the identity.
- **Incremental compiler queries.** Memoise parsing, resolution, inference,
  checking and lowering per binding with dependency tracking, so an edit
  re-infers only what depends on it (a comment edit invalidates nothing
  analytical; a target change invalidates only lowering and target-specific
  evidence). Valuable for agents submitting many speculative edits.
- **Evidence as reusable indexes.** Validation already builds structures:
  key-to-row maps for uniqueness, validity bitmaps, key membership for join
  coverage, block min/max for bounds, dictionaries for distinct counts,
  sorted orders for quantiles. Retain those that later operations or the
  viewer can use, under a cost-based, byte-bounded policy.
- **Incrementally maintained results** for interactive parameters, such as a
  filter threshold answered from a sorted index and prefix aggregates. First
  for viewer interactions only, and only where order and floating-point
  results are unchanged (prefix sums of floats are not).

## Verification

Each change compares full typed results, diagnostics and evidence with the
unoptimised path on the conformance and differential suites, tests
cancellation, eviction and multiple consumers of one result, and reports
memory and latency before and after on the same machine.
