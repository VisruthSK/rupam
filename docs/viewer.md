# Viewer implementation notes

Run `rupam repl --target python-polars examples/viewer-penguins.rp` from
the repository and enter `:view penguins`. This opens 344 observations
with missing measurements. Move to `body_mass` and press `d` for its
distribution, or use F2 to edit the pipeline below the grid:

```rupam
penguins
    |> filter(body_mass > 4000, missing = skip)
    |> arrange(species, desc(body_mass))
```

Press F5 to save the checked pipeline as `view_result`. Use
`:save examples/penguins-analysis.rp` to write the accepted analysis.

Click a cell or use the arrow keys to select it. The wheel pans the grid.
The toolbar buttons offer the same actions as the keyboard. The vertical
rule marks the frozen key columns when a table has a primary key.
The details panel reports missing and distinct counts. Numeric columns show
type-7 quartiles, sample standard deviation and adjusted sample skewness.
Text and Boolean columns show their most frequent values.

The terminal verifier runs under `scripts/dev integration`. It drives the
real CLI, inspects saved bindings with `rupam-dev transform-ir --binding`,
and runs the accepted analysis in both targets. Remaining viewer work is
tracked in the [roadmap](roadmap.md#3-results-and-the-viewer-before-mvp).
