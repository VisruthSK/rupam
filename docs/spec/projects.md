# Projects

## rupam.toml

A directory containing `rupam.toml` is a project.

```toml
[target]
language = "r"
dialect = "dplyr"

[check]
strict = true
```

- `target.language` and `target.dialect` form one valid pair
  ([Session](session.md#language-and-dialect)); `--target r-dplyr` sets both.
  There is no default; a command that runs or compiles without a target fails. An empty REPL can inspect settings and accept `:target` before execution.
- `check.strict`: defaults to `true`.

`[preview] digits = 2` sets decimal places for Float grid and preview cells
(0 through 17, default 2). Calculations, exports and locks retain full precision.

Resolve each key independently in this order, with later values taking precedence:

1. Built-in defaults.
2. The user's `rupam.toml`: `$XDG_CONFIG_HOME/rupam/rupam.toml` on Unix, falling back to `~/.config/rupam/rupam.toml`; `%APPDATA%/rupam/rupam.toml` on Windows.
3. The nearest enclosing project's `rupam.toml`.
4. Explicit command-line flags and subsequent session overrides.

The global file supplies defaults even outside a project. Project environment discovery still starts from the project's own directory. `:options` reports each effective key, value and winning source, including the file path or session flag/command. An explicitly supplied value retains that origin even when it equals the built-in default. Lock semantic settings by effective value. Display preferences and their origins are presentation metadata.

`[agent] data` bounds what the session protocol and MCP server return:
`schema` (default), `summaries` or `rows` ([Agents](agent-interface.md#data-exposure)).

Settings that change a program's meaning are declared in the program
(`set missing`, `set group_missing`).

## Environments

- `pyproject.toml` or `uv.lock` in the project: Python runs through
  `uv run --project`.
- `renv.lock` in the project: R loads the project's renv library.
- Otherwise the interpreters on the PATH are used.

Previews, `run`, conformance and validation use the same target environment.

## Locks

An analysis is compiled for particular inputs, not for arbitrary future
replacement data. `rupam.lock` records a content-addressed analysis entry
per source and target, including:

- resolved contract and definition files, every input data file and schema,
  and explicit external parameters (SHA-256 of exact bytes or canonical
  values);
- rupam/compiler and specification versions, dialect, project settings,
  interpreter and actual dependency versions, dependency lockfiles and
  package identities (including content/artifact digests for local or modified
  packages), data-dict version, relevant platform, locale, timezone
  database and numerical/RNG environment;
- validation results and discharged data-dependent obligations (numerical
  ones only under the determinism rule in
  [Dialects](dialects.md#checks-and-adapters)), keyed to
  the TransformIR, target, exact input/environment fingerprints and evaluation
  scope; explicitly committed discovered source and pivot schemas, including
  the stage identity, parsing rules, ordered levels and provenance.

Only a target-independent proof may be shared across dialects. A successful
R computation cannot certify a Python computation with different numerical
or representation semantics. A strict schema commitment must be independent of target semantics, or be
verified against both targets before being treated as portable. Discovery from
raw strings in a locked source needs no second interpreter; discovery whose
schema depends on native numeric results requires explicit levels or verification
in both targets. A target-specific proof records the target that established it.

File paths are relative to the project where possible; content, not mtime,
identifies an input. Lock records do not contain raw observations or secrets;
committed schema names and public pivot levels are schema metadata. Restricted
values must not be included in discovered names or lock reports.
Dynamic input paths must resolve to a finite set of declared locked inputs
before strict export. Network or foreign dependencies must be captured as
explicit immutable inputs for a reproducible lock. Output paths are not
inputs; an analysis may not overwrite a locked input.

The first successful `rupam lock FILE` establishes a lock after full source
validation and the relevant checked pure computations. Compilation may
create the initial lock automatically only if no provisional schemas or new
snapshot-specific domain assumptions need acceptance. `rupam lock FILE`
is the explicit acceptance action for these; report discovered schemas and
assumptions before recording them. Subsequent changes to data and settings
require explicit relocking. The lock does not cover program text: editing a
program needs no relock unless the edit reads a new input or needs a new
inferred schema, and the program's digest is recorded in compiled manifests
as provenance only. Recheck downstream expressions against the committed schemas.
`rupam check` may inspect an unlocked program and report pending schema or
value-dependent obligations without executing it; `run` and `compile`
require a current lock, an established input schema in strict mode, and all
necessary obligations discharged or supported by a necessary runtime check.
A changed data file, contract, setting, dependency or compiler invalidates the
relevant entry and cached proofs. `rupam lock FILE` reanalyses it and replaces
only that entry on success. Failed relocking leaves the old lock marked stale.
Recompile after relocking; an old generated script remains tied to its own
lock digest, not whatever lock is newest on disk.

The REPL maintains an exploratory session snapshot and reports stale locks
and affected bindings before executing or previewing stale results. It never
silently blesses changed inputs. `:reload` refreshes the exploratory state;
`:lock` validates and records the current complete source and target. A
changed compiler or target environment requires a fresh worker/reanalysis.
Inferred schemas can be explored without a production lock. `:lock` commits
complete-snapshot discoveries and accepted snapshot-specific proofs; show them
in `:schema` and `:explain`. A failed or incomplete discovery never becomes a
production schema. Cache fingerprints and proofs by content and dependency
identity; avoid repeating unaffected computations when downstream code changes.

Generated scripts embed or accompany an immutable manifest containing their
required input hashes and target environment fingerprint. A compact host
preamble verifies required data and dependencies once before reading or
writing; no rupam executable is required. Source/compiler fingerprints are
provenance for the compiled script, not a requirement to install rupam on the
execution machine. If the original source is present, a changed source does
not update the old script. Read the exact verified bytes (immutable snapshot
or equivalent consistency protection), so a file cannot change between
verification and analysis. Reject a mismatch with a clear instruction to
reanalyse, relock and recompile. The preamble is not a security sandbox or a
claim of resistance to malicious modification. Reuse cached validation only
for the exact snapshot and environment. Clock/random effects are documented
exceptions to fixed values; lock their explicit seeds/configuration and
runtime dependencies, not future draws or timestamps.

## Command line

| Command | |
| --- | --- |
| `rupam`, `rupam repl [FILE]` | REPL |
| `rupam lock FILE` | validate and establish or refresh analysis lock |
| `rupam run FILE` | check lock, compile, execute |
| `rupam compile FILE [-o OUT]` | locked script, source map, manifest |
| `rupam check FILE` | parse and type-check |
| `rupam fmt FILE` | format |
| `rupam schema FILE` | describe a contract, or print an inferred schema as a `schema =` literal |
| `rupam explain NAME` | explain a diagnostic |
| `rupam session` | JSON-RPC session protocol over stdio ([Agents](agent-interface.md#session-protocol)) |
| `rupam mcp [--attach]` | MCP server for the project ([Agents](agent-interface.md#mcp-server)) |
| `rupam lsp` | language server (deferred) |

`--target` and `--no-strict` override `rupam.toml` and are included in the lock.

## Distribution

Self-contained binaries for Linux and macOS (x86-64, ARM) and Windows
(x86-64), with DuckDB linked statically. Generated scripts need their locked
interpreter and packages, including any host lock-verification dependency.
The data-dict CLI is needed for rupam validation/relocking, not for executing
a script whose contract obligations were discharged against the lock.
