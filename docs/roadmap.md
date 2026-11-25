# Roadmap

Start with an analyst opening a dataset, exploring it at the prompt, and keeping the useful steps as a readable program. Build this workflow with R and dplyr first. The [specification](spec/README.md) defines Rupam semantics. The [viewer](spec/tooling.md#live-rupam-preview) edits the same Rupam pipeline that the analyst can save in an `.rp` script and compile to R/dplyr. Breaking interfaces is fine when it simplifies the implementation.

## 0. Make every change checkable

Build the [development tooling](spec/development.md) before further features, so each later step is verified by commands rather than by reading terminal sessions.

- Add `.mli` files. Replace string-typed dialects, types and settings with variants.
- Create the diagnostic catalogue and move every `failwith` and `Error of span * string` onto a code. Tests assert codes and spans.
- Add `rupam debug stages`, `rupam repl --transcript`, cram tests for the CLI and REPL, and `scripts/dev check`. Move `test/test_contract.py` to cram tests.
- Give the conformance runner `pending`/`now passes` reporting and run it on every check.
- Adopt `ppx_yojson_conv`, `digestif`, `ppx_expect` and `logs` where they replace existing code.

## 1. Give the prompt and compiler one session model

- Resolve each submitted item to binding identities and check it once. Feed the same typed core to the reference evaluator, interactive compiler and script compiler. Carry source spans, shapes, optionality, row domains, keys, order and obligations through lowering.
- Start the selected worker with the session. Install each immutable value only after successful execution. A later binding can shadow its name while a closure or open viewer still refers to the earlier value. On edits, invalidate dependants. Replay effects only on explicit reanalysis.
- Separate the host language from its table dialect. The host owns worker startup, host syntax printing and source maps. The dialect owns table lowering, adapters, dependencies and capabilities. Implement R/dplyr through these interfaces. A future R/data.table dialect reuses the R worker and printer. The viewer uses the same object and query protocol with Python/Polars.
- Expose the session model through `rupam session` from the start. The REPL and the protocol call the same operations, and protocol cram tests are the REPL's regression suite.
- Keep the current multiline draft, cursor and selection when browsing history or opening a viewer. Complete at the cursor using the schema of the longest checked prefix. Report pending discovery immediately. Enter executes a complete submission. Surface errors at the original expression and keep the last successful value available.

Use the [performance specification](spec/performance.md) to verify the cost of checking, publishing and scrolling individual revisions.

## 2. Share a table once

- Give each completed table a revision and a public schema. Publish its visible columns on first cross-process use as Arrow record batches in a memory-mapped IPC file. Reuse that mapping for the lifetime of the revision. An R data frame may need conversion on first publication. Keep the resulting buffers until the last viewer, query or worker releases its handle. Close mappings before unlinking files on Windows.
- Run completed pure drafts in the target worker off the terminal event loop. Reuse the pinned input and discard superseded results. Hand the viewer an Arrow result handle. Keep DuckDB an internal data-access detail.

- Replace the per-preview Parquet write/read and per-cell string conversion. Paint the first viewport directly from the relevant batches. Project visible columns, index batch row offsets for jumps and format cells at paint time. Scrolling an existing result should incur zero pipeline executions and zero SQL queries.

## 3. Make the object viewer the working surface

The [implementation notes](viewer.md) trace the shared data path.

- Open a binding, the last result, or a completed pipeline stage. Tables open as a two-axis virtual grid with pinned keys. Vectors open as indexed values and records as named fields. Enter drills into a value and Back restores the previous selection and scroll position. Show type, units, description and missingness alongside column details.
- Support multi-column sort, filter, global search, transposed view and column distributions. Compute summaries on demand and cache them by revision and column. Keep restricted values and derived restricted fields outside the viewer catalog. Distinguish display position from analytical order.
- Build grid transformations as ordinary Rupam expressions. Show the expression as the analyst works. Insert it into the saved prompt draft or copy its lowering for the chosen host and dialect. Use the compiler for semantics and code generation.
- Edit a Rupam expression beside the grid. Use REPL completion, show diagnostics against the draft and update the preview when a pure edit succeeds. Preserve the previous result during an incomplete edit. Return the exact expression to the prompt or save it in a script.

- Exercise the full interaction on Windows, macOS and Linux, with keyboard, vi navigation and mouse. Test wide tables, deep jumps, rapid edits during a grouped query, repeated opens, cancellation and rebinding while an old result is visible. Record pipeline evaluations, bytes converted, batches fetched and retained memory alongside interactive profiles.

## 4. Let agents work in the session

Implement [Agents](spec/agents.md) on the session model from step 1.

- Report effects for every item and draft. Enforce data exposure and redaction in one place for all clients.
- Return diagnostics with fixes and decisions. Add the R idiom diagnostics, starting from errors observed in agent transcripts.
- Add `todo` holes and use the hole report for cursor completion in the REPL and viewer.
- Serve `context`, `describe`, result handles and forks. Add `rupam mcp`, then `--attach` with `:share` so an agent's drafts appear in the analyst's viewer as proposals.
- Exercise an agent session from a clean project to a saved, locked `.rp` analysis, and compare it with the same analysis typed at the prompt.

## 5. Complete everyday analysis by operation family

- Start with exact CSV, TSV and Parquet ingestion, explicit and discovered schemas, parsing, dates and missing values. Carry exact Int64, nominal domains and restrictions across ingestion, Arrow transport, display and export.
- Cover selection, rename, derived columns, filtering, grouping, joins, row and column binding, pivots and ordered windows. Define shared rules for row alignment, key transfer, cardinality, tie handling and guarded evaluation in the typed core.
- Group elementwise functions, reductions, order-dependent transforms and parsers by their shared contracts. Define signatures, promotion, missing policies and result shape once per family. Keep exceptions explicit, including exact integer sums, sample variance under two observations, quantile methods and boundary fill for lag. Cover counts, proportions, location, spread, ranks, cumulative statistics, strings and temporal operations from the [standard library](spec/stdlib.md).
- Use the same declarations for checking, completion, documentation and diagnostics. Each dialect supplies vectorised native expressions and the adapters its semantics require. Compare full results, types, order and failures with the reference evaluator for each family.

## 6. Keep the exploration reproducible

Save accepted Rupam submissions as an editable program. Show discovered schemas before locking. Check the exact input and environment before using snapshot evidence. Compile the same checked stages to readable standalone code with a source map and manifest. Exercise opening a dataset, exploring it in the viewer, inserting a transformation, saving, and running in a fresh worker. Verify full-result agreement and rejection of stale inputs.

Continue the specified grammar, type inference and diagnostics work against the [conformance cases](spec/conformance.md). Defer LSP development. Implement prefix checking and completion for the REPL and viewer first. Track the remaining specified dialect and tooling work separately from the R-first implementation order. Features in [Future](spec/future.md) remain deferred.

## Formalisation

A Lean 4 development of the core calculus, kept in `formal/` and checked in
CI, alongside the implementation rather than after it:

- syntax and typing rules of the typed core: shapes, optional values, sign
  refinements and contextual requirements (coeffects) through higher-order
  calls;
- the guarded-evaluation rule shared by `if`, `match`, `let ... else`,
  `coalesce`, `and` and `or`, including reductions over whole groups;
- the missing-value policy and grouping witnesses;
- type soundness (progress and preservation) for the reference evaluator,
  and the theorem that sign refinements make undefined arithmetic
  unreachable.

The reference evaluator is the executable counterpart; conformance fixtures
check that it agrees with the formal rules on concrete programs.
