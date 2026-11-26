# Separate Rupam's compiler from the platform runtime

**Status:** Proposed engineering brief  
**Source baseline:** Rupam working tree supplied with commit `c9526cf49b7c7cdc16edc6b92144ce5ad8b20f9e`  
**Related brief:** [Extract a minimal Talon-based columnar library](talon-columnar-extraction.md)

## Objective

Make the language compiler independently buildable and usable on native platforms and in browsers. The compiler should accept source code and resolved semantic inputs, produce checked TransformIR, diagnostics and R/Python source, and never require an analytical engine, filesystem, subprocess or terminal merely to compile a program.

Preserve Rupam's language guarantees, including exact integers, evaluation scope, missingness, order, effects and target-specific evidence. Improve interactive latency and memory consumption as part of the separation. R/dplyr and Python/Polars remain the production execution engines.

This issue must be implementable and testable with the existing storage libraries. Replacing DuckDB and nanoarrow, extracting Talon, and optimising its decoding and column kernels belong to the companion brief. Do not make this refactor conditional on that replacement.

## Current coupling

| Location | Observed implementation | Required boundary |
| --- | --- | --- |
| `lib/dune` | One library links Unix, DuckDB, nanoarrow and both C bindings alongside the language modules. | Separate build targets with enforced dependencies. |
| `lib/elaborate.ml` | Reads file schemas and digests directly; imports contracts and inspects contracted data. | Consume resolved schemas, snapshot identities and contract descriptions. |
| `lib/scope.ml` / `.mli` | Environment contains a directory, mutable identifier supply and `Contract.t`. | Logical project context and pure contract descriptions; isolate speculative state. |
| `lib/lock.ml` | Hashes input files and calls contract validation. | Platform-side snapshot verification and evidence commitment. |
| `lib/worker.ml`, `runtime.ml` | Native interpreter discovery, processes and evaluation. | Abstract execution service with native and browser implementations. |
| `lib/session.ml` | Combines revisions, workers, result files, cache ownership and presentation events. | Session coordination over explicit execution and result services. |
| `lib/project.ml`, `document.ml`, `server.ml`, `lsp.ml`, `bin/` | Project, document, protocol and native UI/host entry points. | Keep host I/O outside core; split pure logic from transport where necessary. |

The existing `Backend.BACKEND`, typed TransformIR and language-specific printers provide useful boundaries. Preserve those rather than replacing code generation wholesale.

## Proposed dependency structure

```text
rupam_core
  syntax, lexer/parser, types, elaboration, scope, TransformIR,
  verbs, diagnostics, completion, code generation

rupam_services
  resolved input/contract descriptors, snapshot and evidence identities,
  execution requests, result-handle interfaces

rupam_session
  revisions, dependency tracking, evidence coordination,
  evaluation scheduling, result lifetime policy

rupam_native                      rupam_browser
  native input/contract services    virtual input/contract services
  interpreter processes             persistent Pyodide worker
  native result access              browser result store
  filesystem/project IO             browser project persistence

native CLI / LSP / MCP            browser UI
```

These names are illustrative. Core may depend on pure service types, but must not link their platform implementations. Session and frontends depend on service interfaces; concrete native/browser entry points assemble the implementation. Verify the dependency closure rather than relying on directory names. Keep compiler-only and browser targets free of native libraries, process APIs and terminal dependencies.

Audit `Scope`, `Contract`, `Frontend`, `Host`, `Plan`, `Artifact`, `Completion`, `Target` and backend printers individually. Some contain portable logic that can remain in core after extracting their I/O. Keep the embedded R/Python adapter text available to code generation without launching either interpreter. Do not introduce per-row service dispatch in hot loops.

## Input resolution and evidence

Give the compiler an explicit context for logical paths, resolved schemas, parsing policies, contract descriptions and snapshot identity. A read expression must resolve against that context. Missing information should produce an explicit discovery request or a structured unresolved-input diagnostic, never an implicit filesystem scan. The orchestrator can resolve the request and resume or repeat checking against the resulting immutable context. Select the simplest mechanism that supports incremental checking without duplicated expensive work.

Keep inference and refinement rules in core. Platform services establish facts about data; the compiler consumes those facts and determines which obligations they discharge. Separate observations made during exploration from committed evidence. Inferred schemas used for strict export require the commitment specified in `docs/spec/tables.md`.

An immutable snapshot identifies the exact bytes and parsing rules used to establish schema and data facts. Hashes and schemas computed from separate opens of a mutable path are insufficient. Provide a consistent snapshot to the execution backend, or diagnose inconsistency. Verify bytes at the required trust boundary; modification time, file length or a previously cached hash alone cannot establish that an input remains unchanged.

Use the dependency information already represented by TransformIR identities and stages to define evidence keys. Include the relevant snapshot identities, parsing and contract rules, instantiated computation, evaluation scope and target/runtime environment when the proof depends on target semantics. Invalidate only affected evidence. Preserve the distinction between target-independent data facts and target-dependent numerical checks. An R proof cannot justify a Python obligation unless its independence has been established.

The reference evaluator remains an independent interpretation of TransformIR for conformance, rather than the production proof engine. Evaluate data-dependent obligations over the complete relevant pure stage, without committing output effects. Preview samples and random-dependent observations cannot establish reusable snapshot proofs. Keep necessary runtime checks in exported code according to the specification. Standalone scripts must not acquire lock, hash or environment-verification machinery as a side effect of this refactor.

## Execution and result ownership

Define a coarse-grained execution interface for establishing bindings, evaluating checked stages, collecting evidence, publishing results, cancellation and shutdown. Tag requests and replies with their revision and execution environment. A successful host submission is not equivalent to materialising a lazy result; track which guarantee has actually been established.

Keep completed results behind opaque handles. The session should request schema, row count, projected viewports, profiles and release operations without assuming that the result lives in a local file. A native implementation can use the existing Arrow reader; a browser implementation can retain a completed result in its worker or a browser-side store. Use typed values or bounded columnar batches across the boundary. Preserve the logical schema, refinements, row domain, ordering and evidence as Rupam metadata.

Give each published result an explicit lifetime. Forks, open viewers, in-flight profile requests and undo/redo may retain a result after its cache entry is evicted. Release the backing resource only after its last consumer closes. Bound caches by bytes and resource pressure, not only entry count; include retained host objects, mappings, temporary storage and browser memory as applicable. Cancelled or failed evaluations must release unpublished resources without destroying the last successful result.

## Performance work independent of the storage implementation

### Eliminate repeated discovery and decoding

`Data_file.columns` currently issues a schema query and a full missing-value aggregate. `Elaborate` separately calls `Data_file.digest`; `Lock` also computes digests. The host adapters then reopen input paths. This introduces redundant work and a possible mismatch between the bytes checked and those executed.

Coordinate discovery, hashing and validation around a snapshot service. Cache resolved descriptors and valid evidence across revisions; do not rescan unchanged inputs for completion or type inspection. Reuse a canonical typed snapshot or decoded batches where this reduces total work. Measure whether writing a canonical intermediate pays for itself on repeated execution; avoid forcing a second full materialisation for one-off reads. Required lock verification is not optional. Details of decoder fusion and projected validation belong to the storage brief.

### Bound host materialisation and eliminate redundant evaluation

`lib/runtime/polars.py` uses `pq.read_table` or `pacsv.read_csv`, converts the full Arrow table to Polars, and then makes it lazy. `rp_result` calls `collect()` before writing uncompressed IPC. Thus lazy code generation and a memory-mapped result viewer do not by themselves provide bounded-memory execution.

Evaluate canonical batch ingestion and native lazy scans; preserve the exact parsing contract when doing so. Investigate streaming IPC sinks for supported plans and versions. Retain a correct fallback for operations that require materialisation, and make their memory requirements observable. A sink is not a guarantee of constant memory for joins, sorts or all aggregations. Publish a result only after it is complete and valid, using temporary resources until commitment.

Audit the R path separately; do not assume that dplyr can offer Polars-style streaming. `rp_assert_key` currently collects separate missing-key and duplicate-key queries; `rp_covered` collects anti-join checks per required side. Share valid evidence and common pure upstream computation where sound, and evaluate compatible assertions together without changing failure precedence or effects. `rp_wider` explicitly collects its input; account for that materialisation rather than hiding it in compiler timings.

`rp_show_float` and `rp_formatter` iterate through Python values. Profile these paths on large columns and consider native vectorised equivalents only if formatting semantics remain identical.

### Incremental sessions and draft execution

`Session.establish` already tracks established definition stamps. Preserve that optimisation. `Session.open_draft` starts a preview session without the existing worker, and a cancelled draft can close and rebuild its host. Measure the resulting interpreter startup and checked-prefix replay costs. Reuse a warm isolated draft worker and valid immutable inputs/evidence where possible. Do not replay effects or share mutable host state unsafely. Share only pure work with a compatible scope and environment.

Coalesce edits, cancel superseded requests and reject late replies. Keep previous rows and diagnostics appropriately labelled while work is pending. Completion, cursor movement and type inspection use the checked prefix and never evaluate an expression or scan data. Keep costly compilation, profiling and scans off the UI event loop.

Review the current result key (`Target.name` plus serialised table IR hashed with `Digest`) and the eight-entry result cache in `lib/session.ml`. Use explicit dependency and environment identities where required for correctness, and avoid repeatedly serialising large IR trees just to find an already established result. Account for shadowing, target changes, relocking and effectful computations. Cache hits must not initiate unnecessary host work. Do not reuse results solely because two expressions print identically.

### Presentation and scheduling

Request only visible rows and columns from completed results. Cache reusable viewport and profile data by immutable result and presentation parameters. A request for a column profile must not block the editor or cause the original analysis to run again. Format only visible cells; keep exact integers intact until display. Low-level Arrow indexing and typed profiling algorithms belong to the companion brief.

Follow `docs/spec/performance.md`: OCaml domain parallelism is deferred until after the MVP; use `domainslib` rather than custom pools if introduced. First measure stock OCaml builds. Evaluate OxCaml/Flambda 2 only after profiling shows compiler or column-loop allocations justify the toolchain cost. Retain stock OCaml and browser builds. Do not treat speculative compiler optimisations as a substitute for removing full-table scans and cross-runtime copies.

## Browser MVP integration

Build the same compiler with wasm_of_ocaml. Use a persistent Pyodide worker with a tested Polars 1.33.x compatibility profile; 1.33.1 is the proposed browser baseline, not a claim that every generated operation already works. Keep native dependency resolution independent and test supported native versions. Audit Int128 arithmetic, strict narrowing, pivots, joins, dates, missingness and IPC publication against the actual browser wheel. Probe features or use a tested capability profile rather than assuming version numbers imply equivalent behaviour.

Resolve logical input paths through uploaded files or a virtual project directory. Avoid copying an entire dataset between JavaScript, OCaml and Pyodide merely to discover its schema. The browser input service may use the execution environment for decoding if it establishes the same canonical semantics. Do not require a native storage implementation to compile the core.

Keep results in the worker or transfer columnar IPC once to a browser result store, depending on measured memory and latency. Paging requests address completed results; they must never collect the original query again. Preserve Int64 via typed buffers, BigInt or exact decimal representations, not JavaScript Number. Account for separate WASM memories and buffer ownership; Arrow compatibility does not imply zero-copy transfer.

Generate strict R and Python exports without launching R. Preserve necessary target-specific checks and report unsupported or unresolved obligations. A successful Python evaluation is not proof of R numerical behaviour. Defer webR, MCP and data-dict execution in the browser as specified in issue #2. Worker cancellation may require termination and rebuilding valid pure state if an in-flight host kernel cannot be interrupted.

## Implementation order

1. Record a baseline on the existing native build, including typed conformance results and work counters. Add compiler-only build and test targets.
2. Extract pure descriptors and code generation, inject input and contract resolution into elaboration, and isolate filesystem and lock operations.
3. Extract execution and result services; adapt the existing native worker and storage implementation without changing language semantics.
4. Implement snapshot/evidence identities, incremental invalidation and resource ownership. Measure warm edits, cache hits and cancellation.
5. Reduce redundant host decoding and materialisation; add bounded-result publication where supported.
6. Compile the core to WASM, add the browser worker and virtual input service, then test strict exports and browser conformance.

The storage replacement can proceed against the service interfaces after steps 2 and 3. Neither issue should require landing the other in a single large change.

## Acceptance criteria and measurements

- [ ] Compiler-only native and WASM targets link neither DuckDB nor nanoarrow and contain no native process or terminal implementation. Existing native CLI continues to operate during migration.
- [ ] Compilation with supplied descriptors performs no implicit dataset access, contract process launch or analytical evaluation.
- [ ] Input verification, schema, evidence and execution refer to the same immutable snapshot; stale or incompatible evidence cannot discharge an obligation.
- [ ] Existing active conformance fixtures pass and new architecture tests cover target changes, forks, shadowing, relocking, cancelled work and result retention. Pending specification fixtures are tracked separately, not reported as passing.
- [ ] Repeated completion, type inspection and viewport navigation cause zero analytical evaluations and zero input scans. Warm edits reuse valid unchanged work.
- [ ] Result publication has bounded memory for the supported streaming subset; unsupported plans have documented behaviour. Resource retention and cleanup are tested under cancellation, eviction and multiple consumers.
- [ ] Browser compiler and worker can execute representative supported Polars analyses, preserve exact integers, page completed results and export both languages under strict-mode rules.

Measure cold native startup, compiler-only binary/WASM size, browser initial download and engine initialisation, cold input discovery, warm edits, repeated inspection, wide results, deep scrolling, cancellation during grouping and large-result publication. Record compiler time, host submissions, input scans, hash passes, bytes transferred/copied, peak RSS or browser memory, retained handles, allocations, formatted cell count and p50/p95/p99 interaction latency. Compare full typed results under identical inputs and host versions. Report unsupported workloads and separate cold-cache from warm-cache results. Set quantitative regression budgets against the measured baseline before merging performance changes; no benchmark numbers are assumed in this brief.

## Specification references and limitations

Primary local sources: `docs/spec/performance.md`, `session.md`, `tables.md`, `runtime.md`, `dialects.md`, `guarantees.md` and `conformance.md`; implementation locations cited above. Browser scope comes from issue [#2](https://github.com/VisruthSK/rupam/issues/2).

This is a proposed architecture, not an implemented or benchmarked patch. Module names, scheduling mechanisms and result transport are design choices subject to prototype measurements. The source audit establishes the call sites and existing costs, not their share of total runtime on any particular workload.
