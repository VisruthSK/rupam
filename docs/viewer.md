# Viewer implementation notes

Run `rupam repl --target python-polars examples/viewer-penguins.rp` from
the repository and enter `:view penguins`. The grid has focus; move to `body_mass` and press `d` for
its details, or press `i` to edit the pipeline at the prompt below it:

```rupam
penguins
    |> filter(body_mass > 4000, missing = skip)
    |> arrange(species, desc(body_mass))
```

The grid follows the draft as you type. Shift+Enter adds a line; Enter runs
it. A bare pipeline asks for a binding name; `let heavier = …` names it
directly. Use `:save examples/penguins-analysis.rp` to write the accepted
analysis.

Click a cell or use the arrow keys to select it. The wheel pans the grid.
The toolbar buttons offer the same actions as the keyboard. The vertical
rule marks the frozen key columns when a table has a primary key.
The details panel reports missing and distinct counts. Numeric columns show
type-7 quartiles, sample standard deviation and adjusted sample skewness.
Text and Boolean columns show their most frequent values as bars.
Displayed measurements and numeric summaries use `[preview].digits` from
`rupam.toml` (default 2). Filters use the stored value at full precision.

The terminal verifier runs under `scripts/dev integration`. It drives the
real CLI, inspects saved bindings with `rupam-dev transform-ir --binding`,
and runs the accepted analysis in both targets. Remaining viewer work is
tracked in the [roadmap](roadmap.md#3-results-and-the-viewer-before-mvp).
