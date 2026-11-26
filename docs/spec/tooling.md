# Tooling

## REPL

The REPL is an incremental compiler. Each input is checked locally, compiled
to the session dialect and run in a persistent worker in the project
environment. `rupam compile` emits equivalent chunks in source order for
the locked analysis.

- The worker starts when the REPL starts.
- Types and schemas print from the checker; values print from the worker.
- Each table binding is a variable in the worker. A preview reads a bounded
  page of it.
- Entering a later `let` shadows the old binding; existing closures retain
  captured values. Editing an earlier source item invalidates dependent
  computations. Replay effects only as an explicit reload/reanalysis.
- Inferred tables and discovered pivot schemas can be explored; strict export
  needs an established or explicitly committed schema. Show proposed schemas
  and snapshot-specific evidence on lock; never commit partial previews.
- The REPL tracks lock status, checks for changed inputs and dependencies,
  and marks stale results. It explains what changed and offers `:refresh`
  and `:lock`. Do not silently update a production lock.
- Values print in rupam format in every dialect.
- Worker errors are reported at the rupam source span.
- An input that fails to parse, check or execute does not install a binding
  or corrupt existing values. External effects already performed are not
  necessarily reversible.
- An incomplete input waits for more lines; `:prefix` checks its longest
  complete prefix and prints that schema.

| Command | Effect |
| --- | --- |
| `:options` | effective settings and their sources |
| `:type EXPR` | type, schema, or pending discovery; runs nothing |
| `:schema TABLE` | columns, types, refinements, evidence (static, contractual, snapshot-specific), keys, order, units, contract and lock status |
| `:preview TABLE` | first rows |
| `:view EXPR` | inspect a completed value |
| `:explain TABLE` | plan, validation steps, adapters |
| `:probe EXPR` | on-demand stage or expression statistics over its current snapshot |
| `:code TABLE` | generated code |
| `:target LANGUAGE-DIALECT`, `:language L`, `:dialect D` | change target without replaying effects ([Session](session.md#language-and-dialect)) |
| `:save [FILE]` | write accepted source as a `.rp` file |
| `:undo`, `:redo` | move between analytical revisions |
| `:unlock`, `:refresh` | release the snapshot; adopt changed inputs |
| `:version` | build identity |
| `:lock` | validate and lock the current complete analysis |
| `:status` | current/stale lock and changed dependencies |
| `:validate TABLE` | validate the whole source snapshot |
| `:share` | let `rupam mcp --attach` join this session |
| `:help [NAME]`, `?[NAME]` | commands and keys; with a name, the documentation of a function, verb or command, a table's schema or a binding's type |
| `:bindings`, `:quit` | |

`!print()`, `!view()`, `!schema()`, `!hist(column)` and `!bar(column)` are inspection taps
([Session](session.md#inspection-taps)); `:view` and `:preview` remain as
command forms of the same operations.

The prompt highlights syntax as it is typed, completes from a menu, edits
multiple lines and keeps history.

- Tab opens the completion menu; Up and Down move through it. Enter on a
  highlighted suggestion accepts it and never runs the input.
- While the input is a bare command, the line under the prompt shows its usage.
  Enter on a command that requires an argument inserts a space and waits.
- Ctrl+R searches history backwards for the typed substring. Ctrl+R again
  moves to the next older match, Enter keeps the match for editing, and Esc
  or Ctrl+G restores the original input. Completion offers columns with their types
and units, enum values, and functions with signatures.

Tables print the checker’s exact type beneath each column name in subdued grey,
including nominal names, refinements and optionality. Use distinct styling
for missing values, aligned numbers with configurable preview precision, and a footer counting rows
and columns not shown.

## Viewer

`:view` and `!view()` open a full-screen grid on a completed binding,
expression or pipeline stage, with the REPL prompt beneath it holding that
expression. The prompt is the only editor: the same input, history,
completion and multiline editing as everywhere else. Enter runs the draft,
Shift+Enter inserts a newline, Ctrl+Shift+M inserts ` |> ` (also at the plain
REPL prompt), and the grid live-updates from every complete,
checked, pure draft about 150 ms after typing stops; stale runs are
cancelled and an invalid draft keeps the last good grid with its diagnostic.

The grid has focus on opening. Arrows or `hjkl` move the selected cell,
PgUp/PgDn and space page, `g`/`G` jump to the ends, `s`/`S` sort by the
current column, `=` keeps rows equal to the selected cell (asking for a
missing policy only when one is unresolved), `u` undoes the last edit and `d`
describes the column in a panel beside the grid, with missing counts in the missing-value colour. Each grid action rewrites the prompt's
pipeline as typed Rupam syntax. `i` or Tab moves to the prompt and Esc moves
back; `q` or Esc on the grid closes it and restores the REPL. Enter on a
`let name = …` draft binds it; a bare pipeline first asks for a name. The
binding joins the accepted source saved by `:save`. Keys are not shown by
default: `?` on the grid or `:help` at the viewer prompt lists them. Ctrl+G
opens the draft in `$VISUAL` or `$EDITOR` and formats it on return; a
complete pasted item is formatted the same way. The grid and prompt are
placed by a layout parameter, so the prompt can sit below or beside the grid.

- **Grid:** virtualised rows and columns over the Arrow snapshot; only visible pages
  are materialised. Frozen key columns. Column widths fit content.
- **Headers:** name, type, refinement, units, description, key marker.
- **Column summaries:** missing count, distinct count, min, max, mean and a
  histogram or top-values bar for every column, computed on demand and cached by snapshot revision and column.
- **Column detail:** frequency table, quantiles and distribution for one
  column.
- **Sort, filter and search:** multi-column sort; filters by value, range,
  missingness or regex, with matches highlighted; global search.
- **Pipeline:** every sort and filter is a rupam verb. The viewer shows the
  resulting pipeline and inserts it at the REPL prompt. Display ordering
  and ties without conferring an analytical order on unordered previews.
- **Transposed view:** one row per column with sample values.
- **Navigation:** keyboard-first with arrow and vi bindings, plus mouse.
- **Restricted columns** are not shown.
- Runs in the terminal on Windows, macOS and Linux.

## Probes

On demand, inspect a completed pipeline stage or subexpression without
changing source or committing effects. Show filter retained/excluded/unknown
counts; join match coverage on each side, multiplicities and row counts;
summary present counts per reduction; pivot observed levels, absent and
duplicate cells; mutation newly introduced missingness and lost refinements;
and across expanded columns and names. A sampled probe is labelled as such
and cannot establish a proof. Reuse worker computations where possible.
Restricted values and derived restricted statistics remain hidden. Do not
emit routine warnings for these observations.

The REPL and viewer show the consequences of the last submission
unobtrusively, beneath its result: rows each filter kept and dropped,
unmatched rows on each side of a join, missing values each policy skipped or
propagated, and groups formed. These counts are dim, never warnings, and come
from the evaluation that already ran. A count that changes the reading of the
result, such as a join that matched no rows, is shown first.

## Diagnostics

A diagnostic has a stable code, a source span, related spans, and resolutions. A resolution is a fix that preserves meaning or a decision between edits with different analytical results ([Agents](agent-interface.md#diagnostics-as-data)):

```text
rates.rp:8:28  zero-divisor
  `cases / population` is undefined where population is 0.
  population: Float, sign unknown (data-dict.yaml has no assertion on it)

  |> filter(population != 0)
  rate = ratio(cases, population)
```

- Diagnostics refer to columns, rows, tables, missing values and contracts.
  They never show inference variables. Report code-level facts and offer
  concise code fixes. Do not interpret research questions, statistical
  validity, generalizability or substantive implications.
- `rupam explain zero-divisor` prints the full explanation. All codes are
  defined in one catalogue ([Development](development.md#diagnostic-catalogue)).
- Common R and dplyr forms get a diagnostic naming the Rupam form
  ([Agents](agent-interface.md#r-idioms)).
- An unused table expression is a warning. Explicit missing policies and
  ordinary loss of inferred key facts are not warnings. Hover, `:schema`
  and `:explain` show lost keys, ordering, lifting and the reason for a
  required declaration. Missing-group counts may appear in summaries.
- Lock errors identify changed inputs and request relocking/recompilation.

## Editor support (MVP)

Ship a TextMate grammar for `.rp` files with lexer-aligned scopes for syntax
highlighting.

`rupam lsp` provides diagnostics, hover with types, shapes, refinements,
order, keys, lock status and lifting, schema-aware completion, and go-to-definition.
Automatically check the longest complete pipeline prefix during editing and
use its schema for completion in unfinished stages. Never execute an unfinished
stage or implicitly replay effects. For a discovered schema, use cached full
stage discovery or request an explicit probe; mark provisional schemas as such.
Purely static `:type` runs nothing; if discovery is required it reports the
pending schema until a probe or full exploration evaluates it.

## Programmatic access

The session protocol and MCP server are specified in [Agents](agent-interface.md).
