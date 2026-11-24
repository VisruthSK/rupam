# Tooling

## REPL

The REPL is an incremental compiler. Each input is checked locally, compiled
to the session dialect and run in a persistent worker in the project
environment. `rupam compile` emits the same chunks in the same order.

- The worker starts when the REPL starts.
- Types and schemas print from the checker; values print from the worker.
- Each table binding is a variable in the worker. A preview reads a bounded
  page of it.
- Redefining a binding invalidates its dependents.
- Values print in rupam format in every dialect.
- Worker errors are reported at the rupam source span.
- An input that fails to parse or check does not change the session.
- An incomplete input waits for more lines; `:prefix` checks its longest
  complete prefix and prints that schema.

| Command | Effect |
| --- | --- |
| `:type EXPR` | type, or schema of a table expression; runs nothing |
| `:schema TABLE` | columns, types, refinements, keys, units, contract status |
| `:preview TABLE` | first rows |
| `:view TABLE` | the viewer |
| `:explain TABLE` | plan, validation steps, adapters |
| `:code TABLE` | generated code |
| `:target DIALECT` | session dialect |
| `:validate TABLE` | contract validation |
| `:bindings`, `:reload`, `:help`, `:quit` | |

The prompt highlights syntax as it is typed, completes from a menu, edits
multiple lines and keeps history. Completion offers columns with their types
and units, enum values, and functions with signatures.

Tables print with a type row (`<dbl>`, `<int>`, `<chr?>`), distinct styling
for missing values, aligned significant figures, and a footer counting rows
and columns not shown.

## Viewer

`:view` opens a full-screen table explorer.

- **Grid:** virtualised rows and columns over the worker; only visible pages
  are materialised. Frozen key columns. Column widths fit content.
- **Headers:** name, type, refinement, units, description, key marker.
- **Column summaries:** missing count, distinct count, min, max, mean and a
  histogram or top-values bar for every column, computed in the worker.
- **Column detail:** frequency table, quantiles and distribution for one
  column.
- **Sort, filter and search:** multi-column sort; filters by value, range,
  missingness or regex, with matches highlighted; global search.
- **Pipeline:** every sort and filter is a rupam verb. The viewer shows the
  resulting pipeline and inserts it at the REPL prompt.
- **Transposed view:** one row per column with sample values.
- **Navigation:** keyboard-first with arrow and vi bindings, plus mouse.
- **Restricted columns** are not shown.
- Runs in the terminal on Windows, macOS and Linux.

## Diagnostics

A diagnostic has a stable name, a source span, related spans, and fixes
phrased as analysis steps:

```text
rates.rp:8:28  zero-divisor
  `cases / population` is undefined where population is 0.
  population: Float64, sign unknown (data-dict.yaml has no assertion on it)

  Remove those rows first:     |> filter(population > 0)
  Keep them as missing:        rate = ratio(cases, population)
```

- Diagnostics refer to columns, rows, tables, missing values and contracts.
  They never show inference variables.
- `rupam explain zero-divisor` prints the full explanation.
- An unused table expression is a warning.

## Editor support

`rupam lsp` provides diagnostics, hover with types, shapes, refinements and
lifting, schema-aware completion, and go-to-definition.

## Tests (experimental)

```text
test "regional totals" {
  let t = tribble(region, total; "N", 4.0; "N", 6.0; "S", 5.0)
  let r = t |> summarize(total = sum(total), by = region)
  expect(r, rows = 2)
}
```

`rupam test` runs every test in every dialect.
