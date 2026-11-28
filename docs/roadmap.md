# Roadmap

An analyst opens a dataset, explores it at the prompt and in the viewer, and
keeps the useful steps as a readable `.rp` program that compiles to R/dplyr
or Python/Polars. The [specification](spec/README.md) defines semantics and
[conformance](spec/conformance.md) pins them; this page lists only what is
not yet built.

Rupam is complete when every conformance case is active and passes on both
targets, sections 1 to 5 are built, and the public MVP ships. Today 77 of 156
cases are active; sections 2 and 3 unlock most of the rest.

## 1. Self-contained readers

`0.0.0-alpha.1` shipped for Linux (x86-64, arm64), macOS (arm64, x86-64) and
Windows. What remains:

- Merge the Talon readers (pull request 5): CSV, Parquet and Arrow IPC read
  in OCaml and C, BLAKE3 hashing at 85% of the drive's ceiling, and DuckDB
  kept only as a development oracle.
- The demo recorded again at 1080p30 with the current error rendering.

## 2. Semantic foundations

- Share one canonical input snapshot across preview, proof checking and
  export. Rupam verifies it during development. Standalone scripts consume
  the supplied inputs without lock or snapshot verification (C22, C90, C96).
- One inference scheme for helpers carrying parameter roles, scope,
  alignment and missing-policy requirements through higher-order calls
  (C101, C62), replacing the ordinary-then-contextual retry in `function_`;
  `center` inferring `(Number) -> Float?`.
- Lock evidence from the single read. Checking already reads each input once
  for its BLAKE3 digest and schema, and locking reuses both. What remains is
  the evidence its obligations demand, recorded in the lock as discharged
  obligations with presence and NaN counts and bounds
  ([Projects](spec/projects.md#locks)), at the storage's throughput.
- Presence from evidence: `mean(x, missing = skip)` is `Float?` because a
  group can be empty or all missing; a locked snapshot proving every group
  has a present value refines it to `Float`, and `x ?? mean(x, …)` with it.
- Guarded evaluation in Core: represent both the active rows and the
  enclosing group, so a guarded reduction runs once over the whole group when
  any row selects it and not at all otherwise, together with the row-domain
  and order witnesses that survive projection or replacement. Driven by
  C110, C119, C121 and C123; fixes the dplyr `if_else` length failure in C11.
- Platform separation, remaining steps of the
  [brief](briefs/compiler-platform-separation.md): execution and result
  services behind interfaces, snapshot and evidence identities shared by
  preview, lock and export, bounded host materialisation, then the WASM
  compiler and browser worker.
- Generate differential programs from typed TransformIR rather than source
  text, and shrink each failure into a pending fixture.
- Numeric promotion through captured bindings (`let f(x) = x * m` is
  `(Number) -> Number`) and `Number` as an annotation (C160, C161).
- Range patterns: reject overlapping arms and name each uncovered interval in
  `nonexhaustive-match` (C158, C159).
- Displayed signatures show column-shaped parameters and results (C162).
- Lift functions, operators and scalar builtins over vectors as over columns,
  with a length check and no recycling (C165, C37).

## 3. Analytical core

In order of conformance cases unlocked:

1. Contract validation: required, range, NaN and infinity in sources, keys
   and foreign keys, malformed fields (about 9 cases).
2. Evidence and locking: incremental `:lock`, `:unlock`, `:refresh`, schema
   discovery accepted by lock, and obligations discharged by locked evidence
   (about 13 cases).
3. Reshaping: `bind_rows`, `complete`, `expand`, pivot duplicates and
   `values_fn` (about 7 cases).
4. `across` and the remaining column transforms (C77, C106).
5. Ordered windows: `lag`, `lead`, `slice_min` with total-order obligations
   (5 cases).
6. Strict mode and effect sets (5 cases); restricted columns (3 cases).

## 4. Interactive experience

- An explicit session state machine: base revision, pinned inputs, pending
  work and publication outcome. Specify cancellation or worker loss after a
  write or random draw but before publication, keeping the accepted
  revision, the external effect and worker recovery as separate facts.
- `:probe`, `??term` search, comment-preserving `:save`.
- A TextMate grammar; LSP completion and hover from the shared checker.
- Interactive performance ([brief](briefs/interactive-performance.md)):
  REPL start-up and first result, online viewer sort that publishes the
  visible top-k first, progressive summaries, and an antipattern audit, each
  measured before and after.
- Detached sessions ([brief](briefs/detached-session.md)): check, complete
  and inspect types with no execution engine installed, then attach a target
  without losing checked facts.
- Save and export by backward slicing: `:save result` and `:export result`
  keep only the chosen bindings and their dependencies (C164).

## 5. Public MVP

Documentation; `rupam env` environment setup; realistic end-to-end examples
on both targets.

The last step before release is a whole-codebase audit: every algorithm is
log-linear at worst with small constants, accidental complexity and spec
deviations are removed, systems are simplified, hand-written infrastructure
moves to maintained dependencies where one fits, and tests stay native OCaml,
with R and Python only as reference backends.

## 6. After the MVP

- Explicit conversions `Int()`, `Float()` and `String()` from the
  [conversion table](spec/types.md#conversions), with wholeness established
  by `floor`, `ceiling`, `trunc` and `round` (C40, C49, C64).
- Data-discovered enums and `as_enum` ([Future](spec/future.md)).
- `DateTime`, `Time` and zones; nominal types and units (C33, C34, C95).
- Idiomatic R and Python formatting of generated code beyond line breaking,
  if a light dependency or printer pass suffices.
- Replace `scripts/dev` with a `justfile` (or an equally small runner) as a
  development dependency, so build, test, bench and format commands are short
  and reusable; formatting runs ocamlformat, air and jarl for R, and ruff for
  Python.
- Move dependency management to dune package management (`dune pkg` with a
  `dune.lock`, cached through `setup-dune`) once its Windows support is
  stable, replacing the opam switch, `scripts/lock-deps` and the `_opam`
  cache in CI.
- Splitting each backend into a `HOST` and a `DIALECT` once a language has a
  second table library.
- Modules, package export, test blocks, `repeat`, and the relational
  extensions in [Future](spec/future.md).
- Optional OxCaml release builds for Linux and macOS (Flambda 2, unboxed
  types), skipped cleanly on Windows until OxCaml supports it
  ([brief](briefs/compiler-platform-separation.md)).
- Test coverage with `bisect_ppx` once it supports current `cmdliner` and
  `ppxlib` ([Future](spec/future.md#test-coverage)).
- Lean 4 proofs of the [metatheory](theory/metatheory.md) in `formal/`,
  checked against the reference evaluator on shared fixtures.
- The agent session protocol and its fixture runner: `define`, `remove`,
  `evaluate`, `page`, `fork`, `discard`, `context`, `status`, `cancel`,
  holes and decisions on diagnostics (about 11 cases).
- The analyst MCP on MCP 2026-07-28 ([Agents](spec/agent-interface.md)) and
  the [notebook](spec/notebook.md) over the same session.
