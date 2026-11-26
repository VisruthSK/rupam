# Rupam engineering

The language specification defines semantics; implementation shortcuts do not. Resolve ambiguities explicitly and update the specification when semantics change.

## OCaml

- Give every library module an explicit `.mli`. Keep APIs small; hide mutable representations and enforce invariants through abstract types and checked constructors.
- Represent domain concepts, identifiers, states, effects, diagnostics and protocol messages with types, not strings. Match closed variants exhaustively. Parse and validate at boundaries; never derive semantics from diagnostic text, rendered output or ad hoc source scanning.
- Keep mutation local and resources session-owned. No mutable global session state. Isolate speculative inference; failed, cancelled or superseded work must not corrupt committed state. Bracket resource lifetimes.
- Prefer maintained libraries and direct implementations over custom infrastructure or speculative abstractions.

## Compiler and execution

- One checker, one typed Core, one authoritative definition of each semantic rule. Preserve resolved identities, spans, scopes, row domains, refinements and obligations. Generate native code through typed Core, host ASTs and printers; never re-infer semantics in backends or clients.
- Preserve specified missingness, alignment, cardinality, ordering, numerical behavior, guarded evaluation and effects across targets. Discharge obligations only with sound evidence; never silently substitute backend defaults.
- Keep diagnostics structured; distinguish mechanical fixes from analytical decisions. Derive documentation and protocol schemas from authoritative declarations.

## Performance

- Evaluate each binding once per valid revision. Cache evidence against exact dependencies; invalidate selectively. Bound caches and queues; release unreferenced results.
- Use native vectorized operations and Arrow transport; avoid unnecessary scans, copies, boxing and materialization. Completion and scrolling must not execute analysis.
- Keep hot traversals stack-safe. Coalesce edits, cancel stale work and keep expensive operations off the UI loop. Respect FFI ownership, GC rooting and runtime-lock rules.
- Profile representative native builds before optimizing. Measure allocations, latency and memory separately from backend execution. Defer domain parallelism until after MVP; then use bounded Domainslib pools and deterministic merges.

## Verification

- Run the project's development checks and relevant integration tests. Test invariants, failure atomicity and cancellation. Use expect tests, round trips and property-based differential tests against the reference evaluator.
- Record unsupported semantics as pending conformance cases. Never weaken guarantees, suppress failures or promote unverified outputs. Report actual test and benchmark results.
