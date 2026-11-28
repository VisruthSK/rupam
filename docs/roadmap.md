# Roadmap

An analyst opens a dataset, explores it at the prompt and in the viewer, and
keeps the useful steps as a readable `.rp` program that compiles to R/dplyr,
Python/Polars or DuckDB SQL. The [specification](spec/README.md) defines
semantics and [conformance](spec/conformance.md) pins them; this page lists
only what is not yet built, in the order it is built.

Each milestone ships when everything in it is done, every suite is green on
Linux, macOS and Windows, and CI passes at the tagged commit. `0.1.0` is
complete Rupam: every conformance case active and passing on every target,
except the cases of features listed after `0.1.0` (conversions, date-times,
the agent protocol), which stay pending. Today 77 of 156 cases are active.

`0.0.0-alpha.1` shipped for Linux (x86-64, arm64), macOS (arm64, x86-64) and
Windows.

## 0.0.0-alpha.2: fast, self-contained, helpers that work

1. **Talon readers** (pull request 5): CSV, Parquet and Arrow IPC read in
   OCaml and C, one read per input for digest and schema, BLAKE3 at 85% of
   the drive's ceiling, DuckDB only as a development oracle.
2. **REPL input is never rewritten**: pasted and externally edited input is
   kept exactly as typed; formatting happens only on `:save` and in exports.
3. **Helpers infer once, correctly**: one inference scheme for helpers
   carrying parameter roles, scope, alignment and missing-policy requirements
   through higher-order calls (C62, C101), replacing the ordinary-then-
   contextual retry in `function_`; `center` infers `(Number) -> Float?`, so
   `pearson(x, y)` built from `center`, `sum` and `sqrt` works in `summarize`
   with and without `by`. With it: numeric promotion through captured
   bindings and `Number` annotations (C160, C161), column-shaped signatures
   (C162), and vector lifting with a length check and no recycling (C165,
   C37).
4. **Interactive performance** ([brief](briefs/interactive-performance.md)):
   input-to-display latency measured per stage, REPL start-up and first
   result, the online viewer sort that publishes the visible top-k first,
   progressive summaries, and an antipattern audit. Inspection of finished
   results never waits on the worker
   ([results brief](briefs/result-architecture.md), part 1).
5. **Parallel one-pass ingestion**
   ([brief](briefs/parallel-ingestion.md)): at least 1 GB/s per core and
   5 GB/s for a full in-memory read, bounded memory on files larger than RAM,
   and the native Windows reader at parity with Linux.
6. The demo recorded again at 1080p30.

## 0.1.0: complete Rupam

In implementation order; each step unlocks the conformance cases named.

1. **One snapshot, evidence from the single read.** One canonical input
   snapshot across preview, proof checking and export (C22, C90, C96). The
   read that digests and infers also collects the evidence obligations
   demand: presence and NaN counts, bounds, and distinct values when a column
   has few, recorded in the lock as discharged obligations
   ([Projects](spec/projects.md#locks)).
2. **Evidence and locking**: incremental `:lock`, `:unlock`, `:refresh`,
   schema discovery accepted by lock, and obligations discharged by locked
   evidence (about 13 cases). Presence from evidence (`mean(x, missing =
   skip)` refined to `Float` when every group has a present value).
3. **Exhaustiveness from evidence**: a `match` or `match?` over values
   (strings as well as ranges) whose arms cover the locked snapshot's domain
   is accepted, carrying the uncovered domain as a latent requirement; a new
   value in the data makes the lock stale and the relock fails with
   `nonexhaustive-match` naming it, before anything runs. Range patterns
   reject overlaps and name uncovered intervals (C158, C159).
4. **Contract validation**: required, range, NaN and infinity in sources, keys
   and foreign keys, malformed fields (about 9 cases).
5. **Guarded evaluation in Core**: active rows and the enclosing group
   represented together, so a guarded reduction runs once over the group when
   any row selects it (C110, C119, C121, C123; fixes C11 on dplyr).
6. **DuckDB SQL target** (`sql-duckdb`): a third language run through a
   DuckDB host process, so `rupam.exe` links no C++; the same conformance and
   differential suites, with adapters where SQL semantics differ (integer
   overflow, NULL ordering).
7. **Analytical core**: reshaping (`bind_rows`, `complete`, `expand`, pivot
   duplicates, `values_fn`, about 7 cases); `across` and the remaining column
   transforms (C77, C106); ordered windows (`lag`, `lead`, `slice_min` with
   total-order obligations, 5 cases); strict mode and effect sets (5 cases);
   restricted columns (3 cases).
8. **Session**: an explicit state machine (base revision, pinned inputs,
   pending work, publication outcome; cancellation or worker loss after an
   effect but before publication); demand-driven materialisation and index
   views ([results brief](briefs/result-architecture.md), parts 2 and 3);
   detached sessions ([brief](briefs/detached-session.md)); save and export
   by backward slicing (C164); `:probe`, `??term` search and
   comment-preserving `:save`.
9. **Platform separation** ([brief](briefs/compiler-platform-separation.md)):
   execution and result services behind interfaces, bounded host
   materialisation, differential programs generated from typed TransformIR
   with failures shrunk into pending fixtures.
10. **Editors**: a TextMate grammar; LSP completion and hover from the shared
    checker.
11. **Release**: documentation, `rupam env` environment setup, realistic
    end-to-end examples on every target, and a whole-codebase audit: every
    algorithm log-linear at worst with small constants, accidental complexity
    and spec deviations removed, hand-written infrastructure moved to
    maintained dependencies where one fits.

## After 0.1.0

- The browser: the WASM compiler and a worker running `sql-duckdb` on
  duckdb-wasm ([brief](briefs/compiler-platform-separation.md)).
- Results across sessions, incremental compiler queries, evidence retained as
  indexes, and incrementally maintained results, each once a profile shows
  the need ([results brief](briefs/result-architecture.md), part 4).
- Explicit conversions `Int()`, `Float()` and `String()` from the
  [conversion table](spec/types.md#conversions), with wholeness established
  by `floor`, `ceiling`, `trunc` and `round` (C40, C49, C64).
- Data-discovered enums and `as_enum` ([Future](spec/future.md)).
- `DateTime`, `Time` and zones; nominal types and units (C33, C34, C95).
- Idiomatic R and Python formatting of generated code beyond line breaking,
  if a light dependency or printer pass suffices.
- Replace `scripts/dev` with a `justfile` (or an equally small runner);
  formatting runs ocamlformat, air and jarl for R, and ruff for Python.
- Dune package management (`dune pkg`, `dune.lock`, `setup-dune`) once its
  Windows support is stable, replacing the opam switch, `scripts/lock-deps`
  and the `_opam` cache in CI.
- Splitting each backend into a `HOST` and a `DIALECT` once a language has a
  second table library.
- Modules, package export, test blocks, `repeat`, and the relational
  extensions in [Future](spec/future.md).
- Optional OxCaml release builds for Linux and macOS (Flambda 2, unboxed
  types), skipped on Windows until OxCaml supports it.
- Test coverage with `bisect_ppx` once it supports current `cmdliner` and
  `ppxlib` ([Future](spec/future.md#test-coverage)).
- Lean 4 proofs of the [metatheory](theory/metatheory.md) in `formal/`,
  checked against the reference evaluator on shared fixtures.
- The agent session protocol and its fixture runner (about 11 cases), the
  analyst MCP on MCP 2026-07-28 ([Agents](spec/agent-interface.md)) and the
  [notebook](spec/notebook.md) over the same session.
