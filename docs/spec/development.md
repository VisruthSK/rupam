# Development

Tooling for implementing Rupam. A change to the compiler, REPL or viewer must
be checkable against this specification by a command whose output a person or
coding agent can act on without reading a terminal session.

## Build and check

`scripts/dev check` builds, formats and runs every fast test, and prints one
summary line per suite followed by failures only. On Windows it builds in WSL
from a synchronised copy outside `/mnt/c`. `scripts/dev watch` rebuilds and
reruns affected tests on change. `scripts/dev full` adds the R and Python
dialect suites. Each command exits nonzero on any failure.

## Code structure

- Every library module has an `.mli`. The interface documents the module's
  contract through its types.
- Dialects, logical types, effects, settings, diagnostic codes and protocol
  messages are variants or records, never strings, until they are printed.
- One checker. Scalar and table expressions share resolution, inference,
  refinements and diagnostics. Stage-specific rules live behind one entry
  point that returns a typed core.
- Values cross the worker boundary as Arrow data and typed error conditions,
  not printed text or host error messages matched by substring.

## Stage dumps

`rupam debug stages FILE --stage STAGE` prints one compiler stage for a source
file or a REPL submission:

| Stage | Contents |
| --- | --- |
| `tokens` | tokens with spans |
| `syntax` | syntax tree with spans |
| `resolved` | binding identities, column references, ambiguity decisions |
| `typed` | types, shapes, refinements, scopes and obligations per node |
| `core` | typed core |
| `host` | host syntax tree before printing |
| `code` | generated code with its source map |

Output is s-expressions with spans as `file:line:col-line:col`, in a stable
order, so a dump is a valid expect-test snapshot. `--at LINE:COL` restricts a
dump to the innermost node at a position.

## Diagnostic catalogue

`Diagnostic.code` is one variant with a constructor per code. From it derive
the code's name, severity, resolution kinds, `rupam explain` text, the list in
the protocol's capabilities and the reference page in `docs/`. A test fails if
a code has no explanation or no conformance fixture that produces it. Tests
assert codes, spans and resolution kinds; message wording may change freely.

## Conformance runner

`rupam debug conformance [--only ID] [--dialect D] [--json]` runs the fixtures
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
  and REPL transcripts. `dune promote` accepts intended changes.
- **Viewer frames.** The viewer renders to an in-memory cell grid at a fixed
  size. A test drives it with keys and compares frames as text.
- **Differential tests** (`qcheck`). Generate well-typed core programs and
  matching tables, run the reference evaluator and each installed dialect, and
  compare by the conformance rules. Shrink a failure and write it as a
  `pending` fixture.
- **Round trips.** `fmt` is idempotent and preserves the syntax tree; printing
  a type and parsing it gives the same type.

Python is a test dependency only for the `python-polars` dialect.

## Transcripts and fixture capture

`rupam repl --transcript FILE` reads submissions from a file without a
terminal and writes deterministic output: no colour, fixed width, no timing.
`:fixture ID` in the REPL, and `fixture` in the session protocol, write the
last interaction as a `pending` conformance fixture with its source, public
input, target and observed logical schema, diagnostic or value. Restricted
values are never written.

## Tracing

`RUPAM_TRACE=FILE` records a Chrome trace of parse, check, lower, worker
request, Arrow publication and paint, with the counters required by
[Performance](performance.md): checker invalidations, worker submissions,
input scans, Arrow bytes copied, retained mappings, allocations and formatted
cells. Worker spans include the generated code and the reply's size.
`RUPAM_LOG=worker,checker,lock,viewer` enables textual logs per subsystem.

## Developer protocol

`rupam session --debug` and `rupam mcp --debug` add development operations to
the [session protocol](agents.md#session-protocol): `stages`, `conformance`,
`fixture`, `trace` and `explain-internal` (the typed core and obligations of an
item). A coding agent working on the compiler uses the same server an analyst's
agent uses, against the binary it has just built.

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
