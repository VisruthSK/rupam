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
| `:bindings`, `:help`, `:quit` | |

`!print()`, `!view()`, `!schema()` and `!hist(column)` are inspection taps
([Session](session.md#inspection-taps)); `:view` and `:preview` remain as
command forms of the same operations.

The prompt highlights syntax as it is typed, completes from a menu, edits
multiple lines and keeps history. Completion offers columns with their types
and units, enum values, and functions with signatures.

Tables print the checker’s exact type beneath each column name in subdued grey,
including nominal names, refinements and optionality. Use distinct styling
for missing values, aligned numbers with configurable preview precision, and a footer counting rows
and columns not shown.

## Viewer

`:view` opens a full-screen object viewer on a completed binding, expression or
pipeline stage. Tables open in the grid, vectors by index and records by field.
Drill into a value and return to the same selection. Closing the viewer restores
the REPL draft and cursor. Pin the viewed revision until the analyst switches
inputs, including when a binding is shadowed.

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

### Live Rupam preview

Open a table with its binding or completed pipeline in an editable Rupam buffer beside the grid. For example, edit `birds |> filter(body_mass > 4000, missing = skip)`. The viewer uses the REPL's parser, name resolver, checker, completion and compiler. A grid action edits that same expression. The worker evaluates each complete, checked, pure draft against the pinned bindings in the selected dialect. A draft requiring an effect waits for explicit submission at the prompt.

Keep the last successful grid visible while input is incomplete, invalid or running. Identify the displayed revision and show the current draft's diagnostic at its Rupam span. Coalesce edits and discard stale replies. Publish the new typed result when ready. Render visible Arrow cells on demand. Scrolling reuses the completed result.

Insert the expression at the prompt to continue or save it in an `.rp` script. Closing the viewer restores the original draft and cursor. Generated R is available through the ordinary compiler.

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

## Editor support (deferred)

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
