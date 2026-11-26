# Agents

Analysts and coding agents use the same checked session. The REPL, viewer,
session protocol, MCP server and editor are clients of one session model. Each
client sees the same names, types, diagnostics, effects and redactions; none
has private semantics.

## Session model

A session holds an ordered set of top-level items. Each item has a stable
name, a source span, a revision, its dependencies and its effects.

- An item is a `let` binding, a type, a contract, a `use` declaration or an
  unnamed expression. An unnamed expression is addressed by its revision.
- Defining or replacing an item checks it against its dependencies. Success
  installs a new revision and reports which dependants are now stale. Failure
  installs nothing.
- A draft is an expression checked and, when pure, evaluated against pinned
  revisions without installing anything. Viewer drafts and agent drafts are the
  same operation.
- A fork copies the item graph and refers to the same immutable values.
  Forks evaluate independently and never write to each other. Discarding a
  fork releases only the values no other session pins.
- `save` writes the accepted items as an ordinary `.rp` program in dependency
  order, formatted canonically. The saved program is the only artifact; the
  session transcript is not.

## Effects

Every checked item and draft reports its effects before execution:

| Effect | Examples | Runs without approval |
| --- | --- | --- |
| `pure` | arithmetic, verbs over pinned tables | yes |
| `read` | `read_csv`, `read_parquet` of a declared input | yes, once per input revision |
| `random`, `clock` | `set_seed`, `runif`, `now` | no |
| `write` | `write_csv`, `write_parquet` | no |

A client submits an effectful item explicitly. The MCP server and session
protocol require the client to pass the effect set it was shown; a mismatch
rejects the submission. A session started with `--allow read,write` may
pre-approve effect kinds. No client can approve effects outside the project
directory.

## Data exposure

`[agent] data` in `rupam.toml` bounds what leaves the session through the
session protocol and MCP server:

| Value | Clients receive |
| --- | --- |
| `schema` | names, logical types, refinements, keys, order, contracts, row counts |
| `summaries` | `schema` plus per-column statistics from the viewer's summaries |
| `rows` | `summaries` plus bounded pages of rows |

The default is `schema`. Summaries and rows are opt-in. Summaries contain real
values (minima, maxima, enum levels, frequent values and counts), so a name or
rare category in an unrestricted column can reach the agent and its model
provider. Even `schema` discloses row counts, and repeated queries can infer
more. These levels protect against accidental disclosure; they are not a
guarantee against inference or deliberate exfiltration. `context` and the MCP
server state the exposure level on their first line.

Restricted columns and values derived from them are absent at every level, including taps, probes, logs, generated code, schema discovery, diagnostics, suggestions, summaries and
error text returned by a worker. The REPL and viewer are local and are not
bounded by this setting.

Every reply that returns data takes explicit limits on rows, columns and
characters and states what it omitted (`312 more rows`, `14 more columns`).
No reply is truncated silently.

## Context

`context` returns a compact, deterministic description of the session for a
client that cannot see the screen:

```text
project  examples/  target r-dplyr  strict  lock current  exposure summaries
set missing = error

penguins : Table  344 rows  source penguins.csv (lock 3f2a…)
  species      Species              3 levels: Adelie, Chinstrap, Gentoo
  island       Island               3 levels
  bill_length  Float? (mm)        2 missing  32.1..59.6
  body_mass    Int? > 0 (g)       2 missing  2700..6300
  sex          Sex?                 11 missing
  year         Int                2007..2009

heavy = penguins |> filter(body_mass > 4500, missing = skip)
  depends on penguins; 1 stale dependant: by_island
```

- Types print as Rupam types; summaries print only at `summaries` exposure or
  above. Contract descriptions and units are included.
- Order is fixed: settings, then items in source order, then columns in schema
  order. Equal sessions render byte-identical text.
- The same content is available as structured data.

## Diagnostics as data

Each diagnostic in the [catalogue](tooling.md#diagnostics) has a stable code,
a severity, a primary span, related spans, a message, and a list of
resolutions. A resolution is one of:

- **fix**: a text edit that preserves the author's evident meaning, such as
  correcting `body_mas` to `body_mass` or adding a required import. A client
  may apply a fix without asking.
- **decision**: two or more edits that give different analytical results, such
  as `missing = skip` versus `missing = keep`, or `relationship =
  many_to_one` versus deduplicating the right table. Each option states its
  consequence in schema terms (`drops rows where cost is missing; cost becomes
  Float`). When a snapshot is pinned and exposure permits, it adds the
  observed effect (`drops 11 of 344 rows`). A client must not choose a
  decision silently: an agent asks the analyst or states the choice it made.

Codes, resolution kinds and option labels are part of the stable interface.
Message text is not.

## Holes

`todo` is an expression of any type. A program containing `todo` checks but
does not run, lock or compile. For each hole the checker reports the expected
type, the evaluation scope (scalar, column, group), the columns in scope with
their types, and bindings and standard-library functions that can produce the
expected type, ranked by how few arguments remain unresolved.

```text
penguins |> summarize(m = todo, by = species)
# hole: Float or Int, group scope, missing policy required
#   mean(body_mass)  median(bill_length)  n()  ...
```

Completion at a cursor is completion at an implicit hole, and uses the same
report.

## R idioms

The parser and checker recognise common R and dplyr forms that are not Rupam
and report a fix or decision naming the Rupam form:

| Written | Diagnostic | Rupam |
| --- | --- | --- |
| `%>%` | `r-pipe` | `\|>` |
| `NA`, `NA_real_` | `r-na` | `None` |
| `na.rm = TRUE` | `r-na-rm` | decision: `missing = skip` |
| `df$col` inside a verb | `r-dollar` | `col` |
| `group_by(g) \|> summarize(...)` | `r-group-by` | `summarize(..., by = g)` |
| `if_else(c, a, b)`, `ifelse` | `r-if-else` | `if (c) a else b` |
| `case_when(...)` | `r-case-when` | `match` |
| `function(x) ...`, `\(x) ...` | `r-function` | `fn(x) => ...` |
| `T`, `F` | `r-logical` | `TRUE`, `FALSE` |
| `1:n` outside a selector | `r-range` | `1..=n`; `n:1` is `1..=n \|> rev()` |
| `<-` | `r-assign` | `let` |
| `nrow(t)`, `length(unique(x))` | `r-function-name` | `n()`, `n_distinct(x)` |

These diagnostics never make R syntax valid Rupam. The table is part of the
diagnostic catalogue and grows with observed agent and analyst errors.

## Documentation from the binary

`describe NAME` returns, for any function, verb, type, setting or diagnostic
known to the running binary: its signature with constraints, its missing-value
behaviour, its evaluation scope, the dialects that implement it, a short
description and checked examples. The data comes from the declarations the
checker uses. Examples in the description are conformance fixtures.

## Results

A result reply names the item revision, the lock digest when locked, and the
input digests. A client quoting a number can cite `heavy@4` and later ask
whether that revision is still current. Values are returned as typed data:

- Int as a decimal string, Float as the shortest round-trip decimal,
  `None` as null, dates and times in Rupam literal form.
- A table returns its logical schema, row count, analytical order and a page.
- A failed evaluation returns the Rupam diagnostic at the Rupam span, never a
  host traceback. The host message is available as a related note.

## Session protocol

`rupam session [FILE]` speaks JSON-RPC 2.0 over standard input and output, one
JSON object per line, over the same session library as the REPL. `capabilities`
returns the protocol version, methods, targets, effect kinds, exposure level,
diagnostic codes and standard-library names; the method list is the protocol's
own declaration table.

| Operation | Result |
| --- | --- |
| `context` | the context description, text and structured |
| `capabilities`, `version` | what this binary serves; its build identity |
| `options` | effective settings and their sources |
| `bindings` | accepted names and their types |
| `complete` | completions at the end of a draft, from the REPL's completion |
| `submit` | check and accept source items at an expected revision; their events |
| `undo`, `redo` | move to the previous or next revision at an expected revision |
| `target` | select a language and dialect without replaying effects |
| `preview` | a bounded page of a table expression |
| `check` | diagnostics, type, effects and holes for a draft; runs nothing |
| `complete` | completions at a cursor in a draft |
| `define` | install or replace an item; dependants and effects |
| `remove` | remove an item; dependants that no longer check |
| `evaluate` | run a pure draft or a submitted item; a result handle |
| `page` | rows and columns of a result handle within limits |
| `summarize` | column summaries of a result handle |
| `probe` | a [probe](tooling.md#probes) of a pipeline stage |
| `code` | generated host code and source map for an item |
| `describe` | documentation from the binary |
| `explain` | the full text of a diagnostic code |
| `fork`, `discard` | copy or release a session |
| `save` | write accepted items to a `.rp` file |
| `lock`, `status` | lock the analysis; lock and staleness status |
| `cancel` | cancel a request |

- Every reply carries its request ID and the session revision it observed.
  A mutation whose expected revision is not current fails as `superseded`.
- Errors carry `data.category`: `protocol` (malformed JSON, unknown method,
  invalid params), `diagnostic` (code, message, span), `execution` (host or
  runtime failure), `cancelled` or `superseded`.
- Long operations report progress and accept `cancel`.
- Program output, diagnostics and protocol messages are separate fields.
- Request and reply types are defined once in OCaml; their JSON Schema is
  generated from those definitions and served by `capabilities`.

## MCP server

`rupam mcp` adapts the session protocol to the Model Context Protocol over
standard input and output for the project in the current directory. It is an
adapter, not a second session engine or checker.

### Protocol

- The supported revision is `2026-07-28` only: stateless, with no
  initialisation handshake or transport session. Every request carries the
  protocol version and the Rupam session handle in its metadata. Clients on
  older revisions are refused with a protocol error naming the supported one.
- Analytical state is independent of the transport. Requests address a
  session handle and result handles explicitly, each scoped to the
  authenticated client. A dropped connection keeps the session for a bounded
  idle period; reconnecting with the handle resumes it. After a server
  restart, expiry or release, a handle is `invalid-handle` and the client
  starts a new session.

### Tools

Tools are composed around analytical tasks; the full session protocol remains
available to programmatic clients.

| Tool | Does |
| --- | --- |
| `check` | check a pure draft; optionally evaluate it and return a bounded preview |
| `inspect` | context, schema, item, generated code, documentation or a diagnostic |
| `fork` | open, compare or discard an isolated fork |
| `propose` | submit a source change or effect for analyst approval |
| `lock` | lock status, or lock after approval |

Input and output schemas are generated from the typed protocol declarations.
Results are structured. Errors fall into distinct categories: `protocol`,
`diagnostic` (analytical, with code and span), `execution`, `cancelled` and
`superseded`. Clients never parse message text.

### Authorisation and concurrency

- Every mutating request carries an expected revision and commits atomically
  against it; a stale request is rejected.
- An approval binds the exact checked operation: session, revision, canonical
  source and TransformIR digest, input identities, target, effect, resolved
  destination path and overwrite policy. A changed, stale, expired or reused
  approval is rejected. Acknowledging an effect category is not approval.
- `rupam mcp --attach` joins the analyst's REPL only while `:share` is
  active, over a named pipe on Windows or a Unix socket elsewhere, with a
  per-share token. The analyst can revoke it. Permissions are enforced in the
  session, not by tool descriptions or UI.
- Agents evaluate alternatives in forks pinned to one base revision and
  compare bounded consequences permitted by the exposure level. Accepting a
  proposal rechecks it against the current revision; policies and source
  change only with analyst approval.
- Data values, contracts and documents are untrusted data, never instructions.

### Limits and isolation

Each session has finite limits on forks, result handles, worker memory,
request time, response size, queued requests and concurrent evaluations,
reported in capabilities. Results are released explicitly or when their last
consumer leaves. Cancellation, timeout and disconnect clean up workers;
cancelling native code may require terminating the worker.

Workers run with a minimal environment, the project directory as working
directory and paths resolved after following symlinks and reparse points,
checked at use. Where the platform allows, workers have no network access and
write only to declared destinations. A directory check alone is not a
sandbox; the capabilities reply states exactly which restrictions are in force.

### Resources

`rupam://context`, `rupam://item/{name}`, `rupam://schema/{name}`,
`rupam://code/{name}`, `rupam://diagnostic/{code}`, `rupam://describe/{name}`,
all subject to the exposure level.
