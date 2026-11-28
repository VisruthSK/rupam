# Make the REPL and viewer feel instant

**Status:** Not started. Branch from `main` after the Talon pull request (#5)
merges; one pull request.

## Objective

Every interaction that a person waits on is measured, attributed and brought
to the floor its work allows: REPL start-up, the first result, viewer sorting,
paging and summaries. The code stays small and typed; no speed comes from
skipping a check, caching against an inexact key or weakening a guarantee.

Read first: `docs/spec/performance.md`, `docs/spec/session.md`
(Responsiveness, History, results and revisions), `docs/viewer.md`,
`AGENTS.md`.

## Method

1. Measure before changing anything. Add a reproducible harness under
   `test/oracle/` (beside `ipc_bench`, `csv_bench`) that records, separately,
   checker time, worker start, host execution, Arrow transport, Arrow decode,
   formatting and terminal paint for each scenario below. Report p50 and p95
   over repeated runs and the machine.
2. Fix the largest attributed cost, re-measure, keep the change only if it is
   faster in an interleaved comparison (alternate old and new binaries in
   one session; WSL timings drift between sessions by 1.5x).
3. Each commit states its before and after numbers. Never report a number
   that was not measured on that commit.

## Input-to-display latency

The metric that matters in the REPL and viewer is the time from a keypress to
its result on screen. An Arrow viewport read (about 0.1 ms) is one component
of it. Measure every stage of each interaction:

| Stage | Interval |
|---|---|
| T0 key pressed, T1 event read by Rupam | input delivery, T1 − T0 |
| T2 state updated (scroll, selection, draft) | event processing, T2 − T1 |
| T3 data retrieved (Arrow viewport, completion) | data retrieval, T3 − T2 |
| T4 frame built (formatting, layout, diff) | frame construction, T4 − T3 |
| T5 escape sequences written | terminal output, T5 − T4 |
| T6 pixels shown | perceived latency, T6 − T0 |

Application latency is T5 − T1; report it and each stage separately.

- **Internal timing.** Take T1 to T5 from a monotonic clock (add the
  maintained `mtime` library, not wall time). Record them into a
  preallocated ring buffer and report percentiles after the run; logging per
  frame distorts what it measures. Build the instrumentation so it costs
  nothing when off.
- **External harness.** Drive the real console in Mosaic's emulated terminal
  (`Matrix_test`, already used by `test/terminal`), not a physical display:
  load a fixed Arrow result, send a keystroke with a timestamp, and stop the
  clock when the expected screen state appears. Repeat each operation at
  least 1,000 times and report p50, p95, p99 and the maximum. This covers
  application latency and terminal output processing; it cannot measure pixel
  presentation, so state that T6 is not measured.
- **Operations:** a keystroke in the editor, completion opening, a one-row
  scroll, a page scroll, a jump to the end of 10M rows, a column move, a
  sort request (time to first sorted page), `:hist`, and a resize.
- **Budget:** application latency inside one frame at the display's refresh
  (16.7 ms at 60 Hz, 3.6 ms at 280 Hz, per `session.md`) for every
  operation that needs no new computation, at p99. Work that needs
  computation shows its first frame within the same budget and fills in
  progressively.
- **Update throughput:** under held-down keys and fast scrolling, measure
  frames produced per second and dropped or coalesced input. Rupam must never
  queue frames behind input, and never repaint when idle.

## Scenarios

- Cold `rupam repl` to first prompt, with and without a configured target.
- First `read_csv`/`read_parquet` binding to first painted grid, on 10k and
  10M rows.
- A warm edit of one binding with dependents.
- Viewer: open, scroll to the end, jump, sort by one and two columns, `:hist`,
  summary and frequencies on a 10M-row result.
- Cancellation of a long grouped computation, then a new command.

## Known work

- **Online viewer sort.** Sorting reruns the full `arrange` before anything
  paints (`bin/viewer.ml`, `sort`). Publish the visible page first by running
  the same Rupam expression with a bounded head (a top-k the backends already
  plan in linear time: Polars sort-then-head, dplyr `slice_min`/`slice_max`),
  then finish the full order in the background and swap it in. Keep the last
  good grid, cancel superseded sorts, and page past the first window with
  another bounded query until the full result lands. Viewer sorting remains a
  Rupam expression through the backend (`session.md`); add a conformance case
  that the top-k page equals the leading rows of the full sort, including
  ties and missing values.
- **Start-up.** Profile what runs before the prompt: project discovery, target
  resolution, interpreter checks, any library loading. Move what the prompt
  does not need off its path; start the worker speculatively after the prompt
  appears, without delaying input.
- **First result.** Separate interpreter start, package import and query
  time. Keep a warm worker per session; never share one across sessions.
- **Summaries.** `lib/native/arrow_result.ml` computes histogram, summary and
  frequencies with typed kernels and one radix sort. Compute them in one pass
  per column where possible, cache per result handle, and show progressively.
- **Paint.** Grid rendering must not allocate per cell on scroll; format only
  visible cells, reuse buffers, and never redraw when idle.

## Antipattern audit

Search the session, viewer, console and worker code for, and remove:

- repeated scans of the same data or source, or work redone per keystroke;
- copies between representations that an Arrow view would avoid;
- boxed or list-based hot paths that should be arrays or Bigarrays;
- semantics derived from rendered text, diagnostic strings or ad hoc source
  scanning;
- expensive work on the input or paint loop;
- unbounded caches, queues or retained results;
- worse than log-linear algorithms on data-sized inputs.

Record each finding with its location and measured cost, then fix or explain
it.

## Verification

`scripts/dev check` green, the Windows-native suite green before push, CI
green on every release platform. New behaviour (online sort, progressive
summaries, cancellation) gets expect or integration tests, and the
differential suites keep agreeing with the reference evaluator.

## Constraints

- Release binaries keep no C++ or native build requirements; plain C stubs
  are acceptable.
- Parallel work uses a bounded `domainslib` pool with results merged in a
  deterministic order (`lib/native/parallel.ml`). Idle domainslib workers spin,
  so size pools to the work; profile before adding domains.
- Cross-platform I/O: verify on Windows natively; OCaml's `Unix` positioned
  reads are wrong on mingw.
