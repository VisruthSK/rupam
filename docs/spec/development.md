# Development

Tooling for implementing Rupam. A change to the compiler, REPL or viewer must
be checkable against this specification by a command whose output a person or
coding agent can act on without reading a terminal session.

All of it is internal. The `dev/` executable links the compiler library and is
never installed; `scripts/dev` is its only entry point. The installed `rupam`
command has no debug flags, commands or environment variables.

## Build and check

`scripts/dev check` builds, checks formatting and runs the unit, cram and
conformance tests, printing failures only. `scripts/dev watch` rebuilds and
reruns affected tests on change. `scripts/dev full` adds the R and Python
integration tests. `scripts/dev promote` accepts changed expect and cram
output. Each command exits nonzero on any failure.

On Windows the commands run in WSL against the working tree, with the build
directory outside `/mnt/c`. The `rupam` command runs the binary in that build
directory, so every successful build is immediately the installed version.

## Code structure

- Every library module has an `.mli`. The interface documents the module's
  contract through its types.
- Dialects, logical types, effects, settings, diagnostic codes and protocol
  messages are variants or records, never strings, until they are printed.
- One checker. Scalar and table expressions share resolution, inference,
  refinements and diagnostics. Stage-specific rules live behind one entry
  point that returns TransformIR.
- Values cross the worker boundary as Arrow data and typed error conditions,
  not printed text or host error messages matched by substring.

## Stage dumps

`scripts/dev stages STAGE FILE` prints one compiler stage for a source file:

| Stage | Contents |
| --- | --- |
| `tokens` | tokens with spans |
| `syntax` | syntax tree with spans |
| `resolved` | binding identities, column references, ambiguity decisions |
| `typed` | types, shapes, refinements, scopes and obligations per node |
| `transform-ir` | TransformIR with spans and obligations |
| `host` | host syntax tree before printing |
| `code` | generated code with its source map |

Output is s-expressions with spans as `file:line:col-line:col`, in a stable
order, so a dump is a valid expect-test snapshot. `--at LINE:COL` restricts a
dump to the innermost node at a position. `rupam compile FILE --emit
transform-ir` prints the same TransformIR dump from the installed compiler
without lowering or running anything.

`scripts/dev stages transform-ir --binding view_result FILE.rp` isolates
the latest emitted definition of a viewer result for reproduction.

## Developer MCP

`scripts/dev mcp` serves read-only development tools over stdio using the
2026-07-28 revision. Tools accept explicit source, fixture IDs or spec topics
and share the same implementation as the stage commands. The tool set
includes `check`, `tokens`, `syntax`, `typed`, `transform_ir`, `code`,
`evaluate_reference`, `conformance` for reference fixtures, and `describe`.
Each request is independent. The analyst session tools are specified in
[Agents](agent-interface.md#mcp-server).

## Diagnostic catalogue

`Diagnostic.code` is one variant with a constructor per code. From it derive
the code's name, severity, resolution kinds, `rupam explain` text, the list in
the protocol's capabilities and the reference page in `docs/`. A test fails if
a code has no explanation or no conformance fixture that produces it. Tests
assert codes, spans and resolution kinds; message wording may change freely.

## Conformance runner

The reference evaluator is the `reference` library: a direct interpreter of
typed TransformIR that defines each rule executably. Conformance, differential
tests and `rupam-dev` use it; it is never a target and is not linked into
`rupam`.

`scripts/dev conformance [ID...]` runs the fixtures
in `test/conformance/`. A fixture contains source, public inputs, targets and
the expected logical type, value or diagnostic code, and a status:

| Status | Outcome |
| --- | --- |
| `active` | must pass on the reference evaluator and every listed dialect |
| `pending` | runs; a pass is reported as `now passes` and fails the run until promoted |
| `dialect` | active only on the listed dialects; the reason is recorded |

The runner compares results by the rules in [Conformance](conformance.md) and
prints counts per status, then each failure with expected and actual output and
the dialect it occurred in.

## Tests

- **Expect tests** (`ppx_expect`) for printers, type display, stage dumps and
  the lowering of single expressions.
- **Cram tests** (`test/cli/*.t`) for the command line, the session protocol
  and REPL transcripts. **Integration tests** (`test/integration/*.t`, the
  `@integration` alias) compile and run programs in R and Python and print
  their results, so agreement between dialects is a snapshot diff.
  `scripts/dev promote` accepts intended changes.
- **Viewer frames.** The viewer renders to an in-memory cell grid at a fixed
  size. A test drives it with keys and compares frames as text.
- **Differential tests** (`qcheck`). Generate well-typed TransformIR programs and
  matching tables, run the reference evaluator and each installed dialect, and
  compare by the conformance rules. Shrink a failure and write it as a
  `pending` fixture.
- **Round trips.** `fmt` is idempotent and preserves the syntax tree; printing
  a type and parsing it gives the same type.

The integration tests use the project's `uv` environment for Polars, which
also reads their Parquet outputs.

## Transcripts and fixture capture

Without a terminal, `rupam repl` reads submissions from standard input and
writes deterministic output: no colour, fixed width, no timing. Cram tests use
this directly. `scripts/dev fixture ID FILE` runs a source file and writes a
`pending` conformance fixture with its source and, under `[observed]`, the
logical type, value or diagnostic it produced. The correct outcome goes under
`[expect]` by hand, so a captured failure never counts as a pass. Restricted
values are never written.

## Tracing

`scripts/dev trace FILE` runs a source file or transcript and records a Chrome trace of parse, check, lower, worker
request, Arrow publication and paint, with the counters required by
[Performance](performance.md): checker invalidations, worker submissions,
input scans, Arrow bytes copied, retained mappings, allocations and formatted
cells. Worker spans include the generated code and the reply's size.
`scripts/dev log worker,checker FILE` prints textual logs for the named subsystems.

## Developer protocol

The `dev/` executable can serve the [session protocol](agent-interface.md#session-protocol)
with additional operations: `stages`, `conformance`, `fixture`, `trace` and
`transform-ir` (TransformIR and obligations of an item). A coding agent working on
the compiler uses it against the library it has just built.

## Dependencies

Prefer a maintained library to hand-written code when it builds on the project
switch and in the Windows release build.

| Library | Use |
| --- | --- |
| `ppx_yojson_conv` | JSON for locks, manifests, the worker protocol and the session protocol |
| `ppx_deriving_jsonschema` | JSON Schema for session and MCP tool inputs |
| `jsonrpc` | JSON-RPC message types for the session protocol and MCP server |
| `linol` | language server framing and dispatch when editor work resumes |
| `digestif` | streaming SHA-256 of locked inputs and manifests |
| Menhir incremental API and `.messages` | prefix checking, completion at a cursor, syntax error messages |
| `sedlex` | Unicode lexing and code-point positions |
| `ppx_expect`, `ppx_sexp_conv` | expect tests and stage dumps |
| `qcheck-core` | differential and round-trip tests |
| `logs`, `fmt` | logging and pretty printing |
| `trace`, `trace-tef` | Chrome traces |
