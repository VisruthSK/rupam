# Viewer implementation notes

## One path from a binding to the screen

| Responsibility | Current split | Next change |
| --- | --- | --- |
| Check a submission | The REPL dispatches scalar declarations to `Infer` and table expressions to `Analysis_program` and `Check`. | Give the CLI, REPL and editor one checked-submission entry point. Keep the specialised scalar and table rules behind it. Use its resolved binding identities for both execution and inspection. |
| Obtain the value | Scalar evaluation resubmits earlier definitions. Both dialects rebuild table expressions for preview. | Evaluate once per binding revision. Let each target publish the public Arrow snapshot on first demand. Reuse it across queries, previews and open viewers. |
| Describe a column | `Shape.typ` currently holds four primitive types plus nullability and shape. A physical Arrow type alone cannot distinguish two nominal identifiers. | Pair each result field with its checked logical type and metadata. Use one type printer for schema, completion and the row beneath the header. Retain nominal identity through direct projections and renames. |
| Preview a draft | Current table previews regenerate the plan and exchange pages through Parquet. | Compile the edited Rupam expression in the selected dialect. Evaluate against the pinned binding, then expose its public result as Arrow batches. Cancel obsolete work off the UI thread. |
| Display and edit | The terminal and plain output already use the same page layout. Console evaluation blocks its event loop. | Share viewport, type labels and precision rules between preview and grid. Format only visible cells. Each editor owns its draft, cursor and undo state. Route keys to editor actions before session-wide shortcuts. |

## Work remaining

1. Open a completed table revision and scroll in two dimensions. Preserve its exact logical types, the REPL draft and its selection.
2. Edit Rupam beside the table. Share the prompt's completion, diagnostics and lowering. Exercise incomplete edits, cancellation, effectful expressions and rebinding while a viewer remains open.
3. Make grid actions edit the same pipeline. Insert it at the prompt, save it as a script, compile to R/dplyr and compare the full result in a fresh process.
4. Add column detail, search and object drill-down. Verify bounded memory on large and wide tables and mapping lifetimes on Windows.

Mosaic supplies editable state and configurable shortcut-to-action maps. A Vim normal/insert mode needs state across keystrokes. The installed Lambda-Term vi editor uses a separate editor stack. Keep one editing model across the prompt and viewer; add a motion profile when an adapter can drive Mosaic actions while retaining selection, undo and completion.
