# Session

The session library is the sole owner of accepted source items, binding
identities, the dependency graph, analytical settings, immutable results,
evidence, revisions, locks, forks and resource lifetimes. The CLI, REPL,
viewer, notebook, LSP and MCP server are frontends over it; none holds
competing analytical state. Each session owns its workers.

- Mutations carry an expected revision and commit atomically against it.
  Speculative inference, forks and failed or cancelled work never change
  accepted state. External effects already performed are not rolled back.
- A new `let x` follows lexical shadowing; references resolved to the earlier
  `x` keep that identity. Editing an accepted item is a separate operation and
  invalidates its dependents.
- Checking and evaluation are automatic. The session surfaces diagnostics,
  stale state and unresolved obligations, and explains established facts on
  demand through existing inspection; there is no separate provenance store.

## Language and dialect

A target is a host language and a dialect of that language. They are distinct
typed values in configuration, CLI, REPL, capabilities, workers, locks and
manifests, resolved centrally into a valid pair.

```toml
[target]
language = "r"
dialect = "dplyr"
```

| Language | Dialects |
| --- | --- |
| `r` | `dplyr` |
| `python` | `polars` |

`--target r-dplyr` and `:target python-polars` choose a pair atomically.
Every session runs against a target. `rupam repl` takes it from `--target`,
then the project, then the user's `rupam.toml`. Without a configured target,
the terminal REPL opens a numbered picker. Use the Up and Down arrows or
a number key to move, Enter to select, or Esc to cancel. A non-interactive
REPL without a target fails with `missing-target`.
The chosen interpreter is checked before the prompt appears; the worker
starts on first use.
`:language` and `:dialect` change one part; a language change that leaves no
unambiguous dialect requires a choice. `:options` shows both and their origins.

Changing the target keeps accepted source and target-independent checked facts
and never mutates bindings. A dialect change within a language reuses the
worker and recompiles on demand. A language change starts a worker for the new
language and re-establishes values lazily from pinned inputs; no value crosses
hosts except through the Arrow boundary. Target-specific numerical evidence is
discarded. Writes, random draws and clock effects are never replayed; values
that depend on them stay stale until explicit reanalysis. A failed switch
leaves the previous target in place.

## Saving

`:save [FILE]` and the session `save` operation write the accepted analysis
as an ordinary formatted `.rp` file: accepted items and their dependencies in
source order, with `set` declarations, contracts and intentional effects.
History, failed drafts, transient expressions, inspections, session commands
and unaccepted proposals are excluded. Saving writes source only; it neither
runs nor relocks. Overwriting asks for confirmation and the path is printed.

### Saving and exporting a result by slicing

An analysis is reconstructed from its dependency graph, not from its
execution history. `:save result` and `:export result` take one or more
chosen bindings and compute their backward slice: every binding, contract,
`set` declaration and input those bindings depend on, and nothing else.
Exploratory expressions, superseded bindings and unrelated helpers disappear.

```rupam
let clean = penguins |> filter(body_mass > 0, missing = skip)
clean |> summarize(avg = mean(body_mass))
let heavy = clean |> filter(body_mass > 4000)
heavy |> summarize(count = n())
let result = heavy |> summarize(avg = mean(body_mass), by = species)
```

Saving `result` keeps `penguins`, `clean`, `heavy` and `result`. The export
emits the same slice in the target language, and the emitter may fuse
single-use intermediate bindings into one pipeline when that preserves
names visible to the user and evaluation semantics.

- The slice is sound because Rupam controls effects: writes, random draws and
  clock reads are explicit effects, so a dependency is never hidden in
  mutation. An effectful binding is kept only when a kept binding depends on
  it.
- The saved `.rp` source and the data lock are kept alongside generated code;
  the lock records exactly the inputs the slice reads.
- The full exploratory history may be saved separately, on request, for
  transparency. It is never part of the exported program.

## Inspection taps

A tap is an identity-preserving presentation effect usable at any stage of a
top-level interactive pipeline:

```rupam
let regional = sales
  |> !view()
  |> filter(some(revenue))
  |> !hist(revenue)
  |> summarize(average = mean(revenue), by = region)
  |> !print()
```

| Tap | Shows |
| --- | --- |
| `!print()` | the value; bounded rows and columns for tables |
| `!view()` | the viewer on the value |
| `!schema()` | the schema with refinements and evidence |
| `!hist(column)` | a histogram of a numeric column, binned by the larger of the Freedman–Diaconis and Sturges counts, with round-number axis ticks and the count, missing count, mean, sd and five-number summary beside it |
| `!bar(column)` | a bar chart of a categorical column's value counts, missing included |

- Each tap has type `T -> T` with a presentation effect. It observes the exact
  immutable revision it receives and returns it unchanged: schema,
  refinements, row domain, order, keys and evidence are preserved.
- Column arguments resolve against the stage's schema. Arguments configure
  presentation only; they cannot write, run host code or establish facts.
- Taps are syntax nodes, dispatched in pipeline order without blocking input.
  A presentation failure is its own diagnostic and does not fail the value.
- Taps are allowed only in interactive top-level pipelines, including binding
  right-hand sides. They are rejected in function bodies and batch source and
  are removed by `:save` and compilation. Agent exposure rules apply to them.
- A bare REPL expression displays automatically; a terminal tap suppresses the
  automatic display. A tap is also callable directly: `!hist(sales, revenue)`.

`:` commands control the session; `!` taps inspect values; `set` declares
analytical defaults. Each has distinct highlighting and is unambiguous without
colour.

## History, results and revisions

- Input history (navigation, reverse search) is separate from accepted source.
- Unnamed results are ephemeral. A viewer, fork or other consumer holds an
  immutable handle; the result is released when no consumer holds it, subject
  to a bounded cache.
- `:undo` and `:redo` restore retained revisions, policies and valid evidence
  without replaying effects; undo cannot reverse external writes. Retained
  revisions are bounded; an evicted result is re-established only through a
  valid evaluation. Analytical undo is distinct from editor text undo.
- Forks share immutable inputs, results and evidence. Accepting a fork's
  proposal rechecks it against the current revision and permissions. Human
  and agent forks use the same mechanism.
- Viewer sorting, filtering, search, column visibility and histogram layout
  are presentation state and never change a value or its evidence. Applying a
  transformation edits source through the session.

Keys follow Readline conventions: Tab/Shift+Tab completion, Ctrl+R, Up/Down,
Ctrl+A/E/W/U/K/L, Ctrl+C cancel, Ctrl+D exit on empty input, Esc dismiss,
multiline editing and bracketed paste. Pasted and externally edited input is
kept exactly as written; Rupam formats source only when it writes it (`:save`,
exports, `rupam fmt`). Terminal-reserved keys (Ctrl+Z, Ctrl+S) are not
rebound. Analytical undo/redo has its own discoverable binding.

## Evidence and locking

Ingestion pins each input as an immutable snapshot and establishes its
identity, interpretation and schema once. Evidence accumulates from
ingestion, checking and successful evaluation, bound to input identity,
TransformIR stage, evaluation scope, parsing rules and, where relevant, the
target. Tentative observations are separate from committed evidence; a
preview or nondeterministic reduction never proves a universal fact.

Each completed table is published once as an uncompressed Arrow IPC file in
batches of 65,536 rows and released from worker memory. The session memory-maps
it, indexes batch offsets from the footer and decodes only the batches a page
touches, as zero-copy views; the operating system pages data in and out, so a
result never has to fit in memory. A bounded set of results is kept; evicted
ones are unmapped and deleted. Viewer sorting and filtering run as Rupam
expressions through the selected backend, never as separate SQL.

Compatible full-column checks fuse into one scan: presence, bounds, nonzero,
positivity and exact integer-to-float representability. Implied facts are
derived only under their exact conditions (positive implies nonzero for
present finite values); min and max alone never prove representability above
2^53. Keys, composite keys and join coverage use dedicated algorithms over
shared decoded inputs.

- `:lock` commits valid accumulated evidence and performs only missing or
  invalidated work. On failure the old lock is kept and marked stale.
- Whether a pinned source has changed is a separate check at a sound
  boundary; cached hashes and modification times are not proof.
- `:unlock` releases the snapshot commitment and keeps values and evidence.
- `:refresh` adopts changed external inputs and invalidates their dependents
  without replaying effects. Unlock does not refresh.

## Responsiveness

Input and paint never wait on native computation. Rendering follows the
display's refresh (about 16.7 ms per frame at 60 Hz, 3.6 ms at 280 Hz) with
no fixed cap and no redraw when idle. Grids are viewport-bounded: they fetch,
format and paint only the visible cells (a few hundred of a billion), from
Arrow batches indexed by row offset and projected to visible columns, so
scrolling and jumping cost the same on ten rows or ten million. Scrolling, completion, repaint and histogram
layout reuse completed results and never run analysis. Only results matching
the current revision are published; the last successful view stays visible
and marked stale while a draft is invalid or running.
