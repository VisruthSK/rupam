# Tooling

## The REPL

The REPL is an incremental compiler. Each input is parsed and checked
locally, compiled to the project's dialect, and run in a persistent R or
Python worker inside the project's environment. `rupam compile` emits the
same compiled chunks in the same order, so a preview is the compiled program.

- The worker starts in the background when the REPL opens.
- Types and schemas print immediately from the local checker; values follow
  from the worker.
- Each table binding lives in the worker as a variable (a lazy query in
  Polars). A preview asks that variable for a bounded page, so it computes
  only what the page needs.
- Redefining a binding marks everything that depends on it as stale and
  recomputes on demand.
- Values print in rupam's format, identical in every dialect.
- A runtime error in the worker points at the rupam source through the source
  map.
- An input that fails to parse or check leaves the session unchanged.

### Commands

| Command | |
| --- | --- |
| `:type EXPR` | type of an expression, or schema of a table expression; runs nothing |
| `:schema TABLE` | columns, types, keys, units and contract status |
| `:preview TABLE` | the first rows |
| `:view TABLE` | the full-screen viewer |
| `:explain TABLE` | the plan, validation steps and adapters |
| `:code TABLE` | the generated code for one table |
| `:matches REGEX COLUMN` | a regex's matches against a column |
| `:target DIALECT` | switch dialect for this session |
| `:validate TABLE` | run contract validation now |
| `:bindings`, `:reload`, `:help`, `:quit` | |

### Interface

The prompt follows radian: syntax highlighting as you type, a completion menu,
multi-line editing and persistent history. Completion knows the current
table's columns, their types and units, and enum values from the contract.

Tables print like tibbles: a header row of types (`<dbl>`, `<int>`, `<chr?>`),
missing values styled distinctly, numbers with significant-figure alignment,
and a footer counting rows and columns not shown.

The viewer follows RStudio's `View()`: full-screen, scrolling in both
directions, column types and descriptions in the header, sorting and filtering
that compile to the same verbs, and restricted columns hidden. It and the
prompt must work on Windows, macOS and Linux.

### Unfinished input

An incomplete input waits for more lines. `:prefix` checks the longest
complete prefix of the unfinished input and shows its schema. Holes are
reported with their expected type and are never given a value.

## Diagnostics

Every error has a stable code, the source span, labels on related spans, and
a suggested fix where one exists:

```text
error[E0301]: `cost` may be missing, so this predicate can be unknown
  --> visits.rp:12:12
   |
12 |   |> filter(cost > 0.0)
   |             ^^^^ `cost` is Float64? (not required in data-dict.yaml)
   |
help: say what an unknown predicate means
   |   |> filter(cost > 0.0, missing = skip)
```

`rupam explain E0301` prints the longer explanation. Errors talk about
columns, tables and missing values, never about unification variables.
Unused pipeline results are warnings.

## Editors

`rupam lsp` serves the Language Server Protocol: diagnostics as you type,
hover with types, shapes and lifting, schema-aware completion, and
go-to-definition.

## Tests (experimental)

```text
test "regional totals" {
  let t = tribble(region, total; "N", 4.0; "N", 6.0; "S", 5.0)
  let r = t |> summarize(total = sum(total), by = region)
  expect(r, rows = 2)
}
```

`rupam test` runs every test block in every configured dialect, so a project's
own tests show that the dialects agree on its data.
