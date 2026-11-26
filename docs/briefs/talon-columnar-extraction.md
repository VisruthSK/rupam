# Extract a minimal Talon-based columnar library

**Status:** Proposed engineering brief  
**Source baseline:** Rupam working tree supplied with commit `c9526cf49b7c7cdc16edc6b92144ce5ad8b20f9e`  
**Related brief:** [Separate Rupam's compiler from the platform runtime](compiler-platform-separation.md)

## Objective

Remove production linkage to DuckDB and nanoarrow by extracting a small, high-performance columnar storage library from Raven's Talon. Use it for CSV/TSV and Parquet input, target-independent data evidence and Arrow IPC result access. Reduce native executable size and external build requirements while retaining or improving end-to-end throughput, memory consumption and viewer latency.

Vendor a pinned, deliberately limited private fork. Upstreaming is not a requirement. Keep Nx, the tensor/device infrastructure and general dataframe execution outside the dependency closure. The compiler and browser MVP must not depend on completing a WASM port of the full Raven stack.

Do not turn this library into another analytical execution engine. Polars and dplyr remain responsible for production analytical operations. The companion brief owns compiler separation, snapshot orchestration, host workers, general caching and browser integration. This issue owns the physical representation, decoders, specialised evidence kernels and result-reader performance.

## Why Talon and what to extract

Rupam currently uses DuckDB through one SQL primitive in `lib/duckdb_stubs.c`. `Data_file.columns` describes inputs and scans for nulls/NaNs; `Page.read` is a Parquet pagination helper. `reference/reference.ml` loads input via SQL; `dev/rupam_dev.ml` queries Parquet for test inspection. General analytical execution is delegated to the language backends.

Nanoarrow is used through `lib/arrow_stubs.c` and `Arrow_result` to map completed IPC files, obtain typed cells and scan columns. Its replacement must match its low-copy, random-access behaviour rather than loading an entire dataframe.

Talon's development documentation describes `Talon_csv`, `Talon_parquet` and `Talon_arrow`, projected batch sources, Parquet footer and row-group access, and shared Arrow buffers where compatible. Talon also exposes Nx-based numerical columns and uses Rig buffer and disk facilities. The file-format libraries are therefore candidates for extraction, not drop-in standalone dependencies.

| Retain or adapt | Exclude or replace |
| --- | --- |
| CSV tokenisation, quoting and field decoding | CSV separator/header heuristics for declared readers |
| Parquet metadata, encodings, projected page and row-group decoding | General query planner and dataframe execution |
| Arrow IPC framing, FlatBuffers metadata, batch/column decoding | General tensor and extension-type support outside Rupam's needs |
| Primitive schemas, validity bitmaps, string offsets, dictionaries | Nx tensors, Rig device abstractions and GPU machinery |
| Bounded batch sources and structured errors | Unneeded format writers, JSON reader and other unrelated functionality |
| Required compression codecs via narrow interfaces | Hard dependency on the entire native numerical stack |

At the selected upstream commit, audit actual source imports, Dune libraries, generated files, C stubs, transitive codecs and licences. Extract the smallest buildable closure; record the original revision, file provenance, licence notices and a patch series. Raven's repository declares ISC; preserve its notices and check any third-party code separately. Do not assume that deleting Nx from the manifest removes the representation dependencies. Keep upstream decoder changes reviewable without continually rebasing a fork of the entire Raven monorepo.

## Columnar representation and portability

Provide a narrow read-only buffer abstraction with explicit ownership, offsets and length. Native implementations should support mapped Bigarray-backed storage and bounded owned decompression buffers. A portable byte-backed implementation can be investigated later. Keep file mapping and native acceleration separate from format parsing so browser or compiler-only builds can omit them.

Represent fixed-width values in packed buffers, missingness in validity bitmaps, booleans as bit-packed or efficient byte-backed views, and strings as offsets plus UTF-8 bytes. Support both 32-bit and 64-bit string offsets without widening entire columns by default. Use dictionary views where sound; decode only when necessary. Preserve exact signed Int64 and date representations. Keep logical types, refinements and evidence in Rupam, not in the physical decoder.

Avoid boxed per-cell values, row lists and per-cell strings in full-column scans. Choose specialised typed kernels once per column or batch, rather than dispatching through a general virtual interface for each element. A scalar accessor is useful for inspection and tests but must not become the bulk execution path.

Use checked 64-bit lengths and file offsets with bounded local indices. On 31-bit OCaml/WASM targets, reject or window ranges that cannot be represented rather than truncating them. Specify lifetime rules for borrowed slices and decompression buffers; a batch must not outlive its backing storage.

Compression is a separate engineering decision. Support the Parquet codecs required by the supported input contract, including uncompressed and common Snappy, gzip, Zstandard and LZ4_RAW files where verified. Prefer small, independently linked accelerated codecs on native builds when measurements justify them. Do not link them into an uncompressed IPC-only reader. If eliminating all third-party C code is required, evaluate pure OCaml codecs separately against throughput and compatibility. Removing DuckDB and nanoarrow does not by itself guarantee a C-free executable or WASM compatibility.

## Replace `Data_file.query` with typed input inspection

Replace SQL and stringified row results with schema discovery, projected batch scans and typed access. A suggested interface is:

```ocaml
(* Illustrative, not an existing Talon API. *)
type source
type schema
type batch
type projection
type diagnostic

val schema : source -> (schema, diagnostic) result
val scan : source -> projection ->
  (batch -> (unit, diagnostic) result) -> (unit, diagnostic) result
val close : source -> unit
```

Inputs come from the snapshot service defined in the companion brief. Decoders must inspect the exact immutable bytes provided and return facts bound to those bytes, not silently reopen a mutable path. Supply bounded batch sizes and cooperative cancellation at practical boundaries. Represent unsupported types and malformed inputs as structured diagnostics, with file/field/row information when available.

Migrate the reference evaluator's reader to typed batches while retaining its independent TransformIR interpretation. Keep independent input fixtures and differential decoder tests; a shared parser can otherwise introduce correlated bugs in the reference and production paths. Replace `Page.read` only if still needed; remove it if unused. Adapt the dev tool to typed inspection or retain DuckDB strictly as an optional development oracle, outside production linkage.

## CSV and TSV: implement the full Rupam parsing contract

Talon's documented automatic inference samples the first 16,384 records. This is insufficient for committing an inferred schema under Rupam's specification. The extracted reader must expose explicit CSV/TSV policies and a full-snapshot inference path.

Enforce `docs/spec/tables.md` rather than Talon's defaults. Require the specified UTF-8/header/delimiter and quoting rules; distinguish a quoted empty string from an unquoted missing field. Reject duplicate names and invalid records according to the language contract. Preserve leading-zero identifiers, exact Int64 parsing, strict booleans and dates. Apply the specified inference precedence, explicit-schema validation, NaN normalisation and infinity rejection. Avoid accepting conversions merely because a host library permits them.

Use a streaming tokenizer with bounded buffers and maintain candidate types and relevant facts per column across every record. For example, a textual value appearing after a million integer-looking records must affect the final inferred type. Do not commit based on a sample or silently coerce late incompatible values. A two-pass design over an immutable snapshot is reasonable when the final schema is needed before typed decoding; compare it against a one-pass staged representation rather than claiming one pass is always faster. Keep full-table auxiliary allocations out of inference. Explicit-schema reads can validate and decode in one bounded pass.

Where practical, feed the snapshot hasher and lexical validation from the same sequential byte stream. Hash the original bytes, including separators and quoting, rather than normalised records. Snapshot commitment and trust-boundary verification remain the orchestration layer's responsibility. Do not weaken a required validation for the sake of avoiding I/O.

## Parquet: decode only what is necessary

Read the footer for physical schema, row groups and column-chunk locations. Map supported logical/physical combinations explicitly to Rupam types, including signed integers, booleans, floating types, UTF-8 strings and supported dates. Decide and test the Float32-to-Float mapping and signed integer widening under the language specification; reject unsupported logical annotations, decimal, unsigned values outside the representable range, and nested types unless deliberately supported. A permissive Talon conversion is not sufficient grounds to extend Rupam's type system.

Project columns before reading pages. Decode only the chunks needed to establish the requested facts or read selected values. Use reliable metadata where it logically proves an obligation: a non-nullable physical layout or trustworthy zero null count may establish presence for supported non-floating values. NaN and infinity checks require adequate evidence and often actual decoding; footer min/max alone cannot prove arbitrary domain properties, exact integer-to-float representability or uniqueness. Missing, invalid or insufficient statistics must trigger a scan, not a guessed proof. Verify page encodings, dictionaries, validity and logical annotations across real writers.

Reuse decompressed buffers within a bounded batch lifetime. Benchmark row-group sizing, projected reads, dictionary-heavy strings, nullable columns and each required codec. Avoid loading whole files or materialising unrelated columns just to discover metadata.

## Specialised evidence kernels

Add narrowly scoped columnar operations for facts that can be established directly from validated input or a completed, semantically appropriate pure-stage result. They can accelerate the data-dependent obligation system without reimplementing TransformIR evaluation.

| Evidence | Implementation direction |
| --- | --- |
| Presence, missingness, bounds | Bitmap/typed scans; sound metadata shortcuts |
| Finite values, positivity, nonzero | Fused specialised numeric checks |
| Exact Int64-to-Float64 representability | Exact integer checks, including representable values beyond 2^53; never infer exactness from a lossy cast alone |
| Unique/composite keys | Typed hash or sort-based checks with exact equality and bounded/spill strategy where needed |
| Join coverage/cardinality | Projected key membership and counts, respecting Rupam's missingness/equality rules |
| Schema and contract predicates | Validate supported target-independent facts; delegate arbitrary computed expressions to the appropriate evaluator |

Fuse compatible checks into a single pass over a column or batch; preserve deterministic diagnostic order and required failure semantics. Keep typed keys and counts instead of decimal-string conversion. Large exact key sets may need O(distinct keys) storage or a spillable algorithm; do not promise constant memory for exact uniqueness. A probabilistic filter can reject some candidates or accelerate membership but cannot establish exact uniqueness or coverage by itself.

Return evidence and diagnostics, not a claim that the language obligation is discharged. The compiler/evidence coordinator determines applicability using the exact snapshot, stage, scope and target. Target-dependent reductions, effectful/foreign functions and arbitrary derived-stage evaluation remain with the host. Do not replace the reference evaluator with these kernels or build a general-purpose query planner.

## Replace nanoarrow with indexed Arrow IPC access

The existing C reader maps an IPC file, builds cumulative row offsets and binary-searches batches. Preserve these properties. Start with the uncompressed Arrow file profile Rupam actually emits, including the compatibility settings in `lib/runtime/polars.py`. Inspect both Python and R outputs to determine physical encodings; support their actual primitive, date, string and dictionary layouts. Unsupported encodings must fail clearly. Implement streams or compressed IPC only if required by the agreed result contract.

Expose an indexed file handle with schema, total row count, batch descriptors, binary-search row lookup, projected typed views, column scans and explicit close. Derive cumulative row counts from validated record-batch metadata rather than decoding every array at open time. Arrow footer block descriptors do not themselves contain all row counts; read and validate the relevant message metadata. Handle empty batches, zero-row results, overflow and invalid offsets.

| Operation | Desired cost model |
| --- | --- |
| Open file | Proportional to schema and batch metadata, not number of cells |
| Locate arbitrary row | O(log batch count) |
| Read projected viewport | Proportional to touched metadata, buffers and visible cells; no full-table decoding |
| Sequential scan | Linear in requested values/bytes, with bounded working memory except algorithms requiring retained state |
| Close | Release mappings/buffers after last owner; no data scan |

Cache a bounded number of decoded batch descriptors. Borrow compatible primitive, bitmap and string buffers without copying on native platforms. Avoid widening 32-bit string offsets to 64-bit for an entire column merely to expose a common API. For a ten-million-row string array, the offset buffer alone is roughly 40 MB at 32 bits and 80 MB at 64 bits; temporary coexistence adds further pressure. Keep offset width in the view representation.

Validate lengths, alignment where needed, bounds, integer arithmetic, dictionary references and buffer metadata before exposing unsafe fast paths. Reject malformed/truncated files with structured errors. Test lifetime management under eviction, concurrent viewport/profile requests and cancellation. Native mappings are valid only while owned; a finaliser can be a fallback, not the normal release mechanism.

## Viewer-specific performance work

The existing code provides concrete optimisation targets:

| Location | Observed cost | Proposed change |
| --- | --- | --- |
| `arrow_stubs.c`, `rupam_arrow_open` | Invokes batch decoder for every batch to establish starts. | Metadata-only indexing; validate and construct views lazily. |
| `arrow_stubs.c`, cell access | Decodes touched batch descriptors on repeated calls. | Bounded descriptor cache and projected buffer access. |
| `Arrow_result.cells`, `Page.of_arrow`, `Page.format_cell` | Stringifies values; floats are subsequently parsed for rounding. | Typed viewport slices; format visible values directly, with no numeric string round trip. |
| `Session.column_summary` | Reopens the result to compute a summary; integer summaries also compute frequencies. | Use a retained safe handle or independent scan cursor, then cache reusable typed statistics. |
| `Arrow_result.summary` | Two-pass histogram, numeric extraction, full sort, distinct counting and further moments passes. | Reuse compatible scans and typed scratch buffers; avoid sorting unless required. |
| `Session.histogram` | Computes a summary and another histogram with selected bins. | Reuse range, counts or sorted data where compatible with the requested histogram. |
| `Arrow_result.frequencies` | Fetches strings in 4096-row chunks and hashes them. | Typed frequency/distinct kernels; format only displayed categories. |

Retain statistical definitions, numerical stability, missing-value handling and deterministic tie ordering. Exact quantiles may use multi-selection or cached sorting; validate interpolation against current behaviour. If both exact distinct count and quantiles benefit from sorting, benchmark a shared typed sort against hashing and selection rather than assuming either is universally faster. Do not compute integer distinctness by converting Int64 values to Float64; adjacent large integers can collapse. Keep exact integer frequency keys, and define any floating display conversion separately. The current session corrects integer distinct counts through an additional frequency scan; replace that redundant work with typed exact counting. Approximate statistics require an explicit product decision and must never silently replace exact evidence.

A histogram with data-dependent bounds generally needs another pass or retained values; avoid promising a one-pass exact histogram without addressing that dependency. Cache reusable profiles by immutable result, column and statistic parameters through the session interface. General UI scheduling, result lifetime policy and worker-side materialisation remain in the companion brief.

## Delivery stages

1. Pin and audit Raven source. Prototype independent CSV/Parquet and IPC build targets without Nx/Rig device dependencies; measure their linked footprint. If extraction is larger than anticipated, compare a restricted IPC reader and a separate Parquet/CSV subset rather than forcing the entire fork.
2. Establish the packed buffer and schema types, typed scans, diagnostic rules and conformance fixtures. Implement full-snapshot CSV/TSV inference and projected Parquet inspection.
3. Replace DuckDB production call sites; retain an independent development comparison path. Benchmark before removing the old implementation.
4. Implement the indexed IPC reader, typed viewport and ownership tests. Compare with nanoarrow before switching result access.
5. Add specialised evidence and profile kernels, fuse compatible scans, and measure memory/throughput. Keep optional codec acceleration independent of the core decoder.
6. Remove obsolete C stubs and production vendor linkage. Verify release artifacts and compiler-only linkage separately. Document upstream pin, patches, supported format subset, known limitations and upgrade procedure.

## Acceptance criteria and benchmarks

- [ ] Production CLI no longer links DuckDB or nanoarrow. The extracted library's dependency closure excludes Nx, Rig device/tensor infrastructure and unrelated Raven packages; any retained codecs or minimal platform stubs are documented.
- [ ] CSV/TSV inference covers the entire committed snapshot. Adversarial late values, integer boundaries, leading zeros, quoted empties, invalid UTF-8, duplicate names, malformed records, NaNs and infinities have positive and negative fixtures as applicable.
- [ ] Parquet tests cover required logical types, row groups, projection, nulls, supported codecs and encodings, absent/misleading metadata and malformed files. No metadata shortcut discharges an unsupported obligation.
- [ ] Reference evaluator semantics remain independent; compare decoded typed fixtures with an independent implementation and known expected values. Current active conformance fixtures pass; track pending specification work separately.
- [ ] IPC tests cover both host writers, multi-batch/empty results, deep random access, string offset widths, nulls, large exact integers, dictionaries as emitted, malformed metadata and ownership. Opening and paging never materialise the full table.
- [ ] Evidence kernels agree with straightforward exact reference checks on boundary cases and preserve target/scope restrictions. Profile results agree with existing statistical definitions, except explicitly documented bug fixes.
- [ ] Native release size and end-to-end performance meet regression budgets established from measured baselines. No unmeasured claim of speedup or portability counts as acceptance.

Benchmark existing DuckDB/nanoarrow and the replacement on identical inputs, hardware, cache state and host versions. Include large numeric and string-heavy CSV, late type changes, wide and projected Parquet, each required codec, high-cardinality/composite keys, multi-batch Arrow results and deep scrolling. Suggested stress cases include 1 GB input and 100 million rows where hardware permits; scale down with explicit reporting rather than risking out-of-memory failure.

Record ingest and evidence throughput (bytes/s and rows/s), CPU time, peak RSS, OCaml allocations, decompressed and copied bytes, number of full scans, mapping size and residency separately, open latency, viewport p50/p95/p99, exact profile time and temporary memory. Measure stripped and compressed release size, cold startup and transitive linked dependencies. Compare both per-kernel and end-to-end results; reducing executable size does not justify a major throughput regression. Set numeric targets after recording baselines.

## References and unresolved decisions

Local specification: `docs/spec/tables.md`, `runtime.md`, `session.md`, `dialects.md`, `performance.md` and `conformance.md`. Implementation: `lib/data_file.ml`, `duckdb_stubs.c`, `arrow_stubs.c`, `arrow_result.ml`, `page.ml`, `session.ml`, `reference/reference.ml`, `dev/rupam_dev.ml` and `lib/dune`.

Upstream references (development documentation; pin source before relying on APIs):

- [Raven repository and licence](https://github.com/raven-ml/raven)
- [Talon overview and Nx relationship](https://raven-ml.dev/docs/talon/)
- [Talon file formats and CSV inference](https://raven-ml.dev/docs/talon/formats/)
- [Talon Parquet API](https://raven-ml.dev/docs/talon/api/Talon_parquet/)
- [Talon Arrow API](https://raven-ml.dev/docs/talon/api/Talon_arrow/)

The extraction has not been compiled or benchmarked. The precise file-level dependency closure, source revision to vendor, codec strategy and supported physical format subset require a prototype and inspection of actual host outputs. A native-only minimal extraction is acceptable for the first migration; a portable buffer/parser target can follow without making browser delivery depend on a full Talon port. The core compiler remains independent of the storage implementation.
