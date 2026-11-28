# Separate Rupam's compiler from the platform runtime

**Status:** Remaining work

## Objective

The `rupam` compiler library builds without DuckDB, nanoarrow, processes or a
terminal, and receives input facts through `Scope.inputs`. What remains is
the rest of the platform boundary: execution and results behind interfaces,
one snapshot identity shared by preview, lock and export, bounded host
materialisation, and the browser build. R/dplyr, Python/Polars and, once
built, DuckDB SQL are the production engines; the browser runs DuckDB SQL on
duckdb-wasm. Language guarantees (exact integers, evaluation scope,
missingness, order, effects, target-specific evidence) are unchanged.

## Execution and result services

- Define a coarse execution interface in a pure services library:
  establishing bindings, evaluating checked stages, collecting evidence,
  publishing results, cancellation and shutdown. Tag requests and replies
  with their revision and execution environment. A successful submission is
  not a materialised result; track which guarantee was established.
- Keep completed results behind opaque handles. The session asks for schema,
  row count, projected viewports, profiles and release without assuming a
  local file. The native implementation keeps the Arrow reader; a browser
  implementation keeps results in its worker or a browser store.
- Give each result an explicit lifetime. Forks, open viewers, in-flight
  profiles and undo/redo may retain a result after cache eviction; release the
  backing resource only after its last consumer. Bound caches by bytes and
  resource pressure, not entry count. Cancelled or failed evaluations release
  unpublished resources without destroying the last successful result.
- Move `Session` onto these interfaces with native implementations in
  `rupam_native`, so frontends assemble the implementation instead of linking
  workers directly.

## Snapshot and evidence identity

- An immutable snapshot identifies the exact bytes and parsing rules used to
  establish schema and data facts. Preview, proof checking, lock and export
  read that snapshot; separate opens of a mutable path are not consistent.
  Modification time, file length or a cached hash never prove an input is
  unchanged.
- Key evidence by snapshot identities, parsing and contract rules, the
  instantiated computation, evaluation scope and, for numerical checks, the
  target. Invalidate only affected evidence. Separate exploration
  observations from committed evidence.
- A missing input produces an explicit discovery request or structured
  unresolved-input diagnostic; the orchestrator resolves it and rechecks
  against the resulting immutable context.

## Performance

- Discovery, hashing and validation share one pass per snapshot.
  `Data_file.columns` currently issues a schema query and a full missing-value
  aggregate, `Lock` hashes again and the host adapters reopen the files.
- Bound host materialisation: evaluate native lazy scans and streaming IPC
  sinks for supported plans, with a correct materialising fallback whose
  memory is observable. `rp_wider`, `rp_assert_key` and `rp_covered` collect;
  account for that and share upstream work between compatible assertions
  without changing failure precedence.
- Reuse a warm isolated draft worker and immutable inputs instead of
  rebuilding the host after a cancelled draft; never share mutable host state
  or replay effects.
- Replace the result cache key (target name plus serialised IR digest) with
  explicit dependency and environment identities, and avoid serialising large
  IR to find an established result. Cache hits start no host work.
- `rp_show_float` and `rp_formatter` iterate Python values; vectorise only if
  formatting stays identical.
- The viewer's sort and summary statistics are too slow on large inputs:
  opening the 2M-row `trips.parquet` and sorting by `fare` stalls. Sort
  online: compute the visible window first (top-k for the viewport) and
  publish it immediately, then finish the full order in the background and
  buffer further pages only if paging outruns it. Compute summary statistics
  with one vectorised pass per column, cached per result, and show them
  progressively instead of blocking.
- OxCaml (issue 3): OxCaml is based on OCaml 5.4, so lower the minimum
  supported OCaml to 5.4 (the source uses nothing from 5.5) while building
  releases with 5.5. Later, build Linux and macOS releases with OxCaml
  (Flambda 2 at least, unboxed types where they pay) as an optional
  configuration that Windows builds skip cleanly, with no OxCaml-only code
  paths in the shared source, until OxCaml supports Windows; then move
  everything to OxCaml.
- Profile before adopting OxCaml or Flambda 2; domain parallelism stays after
  the MVP with `domainslib`.

## Browser build

- Build the compiler with wasm_of_ocaml against the pure libraries.
- Run a persistent duckdb-wasm worker executing the `sql-duckdb` dialect,
  with a tested capability profile for the pinned duckdb-wasm release; audit
  integer overflow, strict narrowing, pivots, joins, dates, missingness and
  Arrow IPC publication against it, since duckdb-wasm can lag native DuckDB
  in extensions and behaviour.
- Resolve logical inputs through uploads or a virtual project directory,
  registered with duckdb-wasm as files, without copying whole datasets
  between JavaScript, OCaml and the worker to discover schemas.
- Keep results in the worker or transfer columnar IPC once; paging addresses
  completed results and never re-runs the query. Preserve Int64 exactly with
  typed buffers or BigInt.
- Generate strict R, Python and SQL exports without launching R or Python.
  Defer Pyodide (Polars), webR, MCP and data-dict execution in the browser.

## Order

1. Execution and result services; adapt the native worker and storage.
2. Snapshot and evidence identities, incremental invalidation, result
   lifetimes; measure warm edits, cache hits and cancellation.
3. Shared discovery pass and bounded host materialisation.
4. WASM compiler, browser worker and virtual inputs; strict exports and
   browser conformance.

## Acceptance

- The WASM compiler target links no native code and performs no implicit
  dataset access, process launch or evaluation when given descriptors.
- Verification, schema, evidence and execution refer to one immutable
  snapshot; stale evidence cannot discharge an obligation.
- Architecture tests cover target changes, forks, shadowing, relocking,
  cancelled work and result retention.
- Completion, type inspection and viewport navigation cause zero evaluations
  and zero input scans; warm edits reuse unchanged work.
- Result publication has bounded memory for the streaming subset, with
  documented behaviour otherwise; cleanup is tested under cancellation,
  eviction and multiple consumers.
- The browser compiler and duckdb-wasm worker run representative analyses,
  preserve exact integers, page completed results and export all three
  languages.
