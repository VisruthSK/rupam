# Roadmap

An analyst opens a dataset, explores it at the prompt and in the viewer, and
keeps the useful steps as a readable `.rp` program that compiles to R/dplyr
or Python/Polars. The [specification](spec/README.md) defines semantics.
Architecture comes first: every client consumes one session through one path.

## 1. One session, one path

The [session](spec/session.md) library owns accepted items, identities,
revisions, the worker, results and evidence. The REPL and `rupam session`
are thin clients over its typed operations; the viewer, MCP and notebook
join them without a second engine. Remaining: `complete`, `context`, forks
and result handles in the protocol, as steps 3 and 6 need them.

## 2. Language pass

`Int`/`Float`, `Core` renamed `TransformIR`, `set`, `??` and variadic
`coalesce`, `match?`, `in`/`not in`, positional `first`/`last`,
`assert_key` requiredness, the `keep` policy across every reduction. Conformance fixtures move with each change.

## 3. Results and the viewer (before MVP)

- Completed results as immutable Arrow batches behind result handles:
  one collect per Polars binding, one pass per preview, no Parquet or string
  round trip. Scrolling runs no analysis.
- Full-screen viewer on any binding, stage or tap: virtual two-axis grid,
  pinned keys, drill-in and back, sort, filter, search, distributions cached
  per revision. Paint at the display's refresh with no idle redraw.
- `!print`, `!view`, `!schema`, `!hist` taps as syntax nodes.
- Grid transformations edit Rupam source through the session.

## 4. Evidence and locking

Snapshots pinned at ingestion; evidence cached per input and stage; fused
validation scans; incremental `:lock`, `:unlock`, `:refresh`.

## 5. Everyday analysis

Ingestion, verbs, joins, pivots, windows and the standard library by
operation family, each checked against the reference evaluator.

**MVP:** steps 1–5 on R/dplyr and Python/Polars, Linux, macOS and native Windows.

## 6. Agents

`rupam mcp` on MCP 2026-07-28 over the session, with composed tools,
exact approvals, revision checks, exposure limits and `:share`
([Agents](spec/agent-interface.md)).

## 7. Notebook

The [notebook](spec/notebook.md) over the same session and result transport.

## Formalisation

Lean 4 in `formal/`, alongside the implementation: TransformIR typing,
guarded evaluation, missing policies, and soundness of the reference
evaluator. Conformance fixtures tie it to the implementation.
