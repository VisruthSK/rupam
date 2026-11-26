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

The terminal grid projects bounded Arrow pages, keeps the active cell inside
an independent viewport, and freezes the primary key. The live Rupam editor
shares the REPL's checker and completion. Grid actions edit its pipeline,
and saving installs a binding in the accepted source. Remaining:

- Result ownership: consumers retain results; eviction and target switches
  never unmap a retained handle. Grid steps run against the retained result,
  not by re-executing the source pipeline.
- Extend typed cell and literal transport as the compiler adds nominal and
  temporal columns.
- Add dedicated controls for range filters, projection, counts and search.
- Measure frame time and input latency on wide and long tables and cache
  formatting where the measurements warrant it.

## 4. Evidence and locking

Snapshots pinned at ingestion; evidence cached per input and stage; fused
validation scans; incremental `:lock`, `:unlock`, `:refresh`.

## 5. Everyday analysis

Ingestion, verbs, joins, pivots, windows and the standard library by
operation family, each checked against the reference evaluator.

### Backend interfaces (before MVP)

Require each executable target to satisfy a shared OCaml `BACKEND` signature.
Expose the target, typed capability declarations, naming, preamble, script and
source-map generation, incremental statements, and result materialization.
Register the checked backend once so those operations share one dispatch.

Define `HOST` for host ASTs, scalar lowering, printing, source maps and worker
integration. Define `DIALECT` for native table and column lowering, capability
declarations and analytical adapters. Share a host across its dialects. Use a
functor with shared abstract AST types to assemble compatible pairs, so an
R dialect cannot pass R nodes to a Python printer.

Migrate R/dplyr and Python/Polars before adding another target. The signature
checks the interface. Shared conformance against the reference evaluator
checks missingness, numeric behavior, guarding, grouping, row alignment,
cardinality and ordering.

## 6. Editor support (penultimate MVP step)

Ship a TextMate grammar for `.rp` files and an LSP that uses the shared
Rupam checker. Cover live diagnostics, completion, hover, go-to-definition
and formatting. Test syntax scopes against language fixtures and exercise
incremental LSP requests without evaluating analyses during editing.

## 7. Developer MCP (final MVP step)

Serve read-only compiler tools from `scripts/dev mcp` over stdio. Reuse
the stage dumps, checker, reference evaluator, fixture verifier and spec.
Exercise discovery, tool schemas and structured diagnostics in tests. The
analyst MCP with session handles, forks and approvals is step 9.

**MVP:** steps 1–7, including the backend interfaces and developer MCP, on
R/dplyr and Python/Polars for Linux, macOS and native Windows.

## 8. Formalize the reference evaluator (first after MVP)

Build the Lean 4 model in `formal/` before starting the next roadmap steps.
Connect executable Lean cases to the OCaml reference evaluator through shared
conformance fixtures. Start with a precise model of evaluation and failure,
then prove:

- Type preservation and isolation of incompatible observation domains.
- Correct lifting over scalars, optional values and row-aligned columns.
- Preservation of established bounds and presence, and exhaustiveness over
  the domains accepted by the checker.
- In a guarded choice only selected computations can fail. Selected
  reductions read their entire required group.
- Missingness policies implement the operation-specific rules in the
  specification.

## 9. Agents

`rupam mcp` on MCP 2026-07-28 over the session, with composed tools,
exact approvals, revision checks, exposure limits and `:share`
([Agents](spec/agent-interface.md)).

## 10. Notebook

The [notebook](spec/notebook.md) over the same session and result transport.
