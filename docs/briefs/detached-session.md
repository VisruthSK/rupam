# Detached sessions: check without an execution engine

**Status:** Not started. One pull request from `main`.

## Objective

A person can open Rupam with no R or Python installed, write an analysis and
reason about it through types: bindings check, schemas propagate, contracts
and obligations are reported, completion and hover work, `:type` and
`:schema` answer. Nothing produces values. A detached session is a session
without a target's execution engine, not a second checker or a reduced
language.

Read first: `docs/spec/session.md`, `docs/spec/guarantees.md`,
`docs/spec/dialects.md`, `docs/spec/projects.md`, `docs/spec/contracts.md`,
`AGENTS.md`.

## Semantics to specify first

Update `docs/spec/session.md` (it now says every session runs against a
target) before writing code, and settle these explicitly:

- **Entry.** How a session becomes detached: an explicit choice (for example
  `rupam repl --detached` and `:detach`/`:target`), and what a REPL with no
  configured target does (today: the picker, or `missing-target` when
  non-interactive).
- **Dialect.** A detached session either has no dialect, so only the portable
  core and strict checks apply, or keeps a chosen dialect for capability
  checking without a worker. Decide which, and what target-specific imports
  (`import polars`) do in each.
- **Input facts.** Schemas come only from sources that need no engine: the
  native readers (`Data_file.columns`, Talon CSV/Parquet), contracts, and
  schemas recorded in `rupam.lock`. State which are allowed and whether
  reading a data file's schema counts as execution (it does not run host
  code, and it must be the same inference the attached session uses).
- **Obligations.** Obligations that need evidence stay unresolved and are
  shown as such; they are never discharged or assumed. Target-specific
  numerical evidence does not exist.
- **Disabled operations.** Preview, `:hist`, the viewer, evaluation, export
  that runs code, and lock refresh report one structured diagnostic naming
  the missing engine, with a mechanical fix (choose a target). Saving source
  works.
- **Attach.** Choosing a target later attaches an engine: accepted source and
  target-independent facts are kept, values are established lazily, as a
  target change does today.

## Implementation outline

- Make the target optional in the session rather than adding a separate mode:
  `Project.require_target` call sites (`lib/native/session.ml`,
  `lib/native/lock.ml`, `bin/rupam_cli.ml`) split into operations that need an
  engine and those that do not. Represent the state with a type (for example
  `Attached of target | Detached`), matched exhaustively, never a flag or a
  string.
- The checker already runs against `Scope.inputs`; feed it the same native
  schema sources. Do not add a second inference path.
- Worker start stays lazy; a detached session never spawns one.
- Frontends (REPL, LSP, MCP) show the detached state and the unavailable
  commands from the session's typed capabilities, not by catching failures.

## Verification

- Expect tests: a detached REPL checks a multi-step pipeline, reports a type
  error, shows `:schema`, and rejects preview with the structured diagnostic.
- An integration test with no interpreter on `PATH`.
- Attach after detached work and confirm the same checked facts and values as
  a session that started attached.
- Conformance: the checker's results are identical attached and detached for
  every active case that does not evaluate.
- `scripts/dev check`, the Windows-native suite and CI green.
