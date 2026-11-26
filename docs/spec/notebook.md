# Notebook

`rupam notebook [FILE]` starts a local server, opens the default browser and loads an ordinary `.rp` program. The notebook is an alternative frontend to the REPL and viewer, with the same project settings, target, compiler, [session](session.md) and diagnostics. Its workflow is reactive, in the spirit of marimo. Notebook support is deferred until the shared session and result transport are established.

## Architecture

Use Dream and Lwt for the native server, subject to native Windows conformance. Serve embedded browser assets and bridge the existing typed session protocol over WebSocket. The server calls the session library directly. Use Bonsai_web, CodeMirror and Bonsai partial-render tables, compiled with js_of_ocaml. Pin compatible frontend dependencies in a separate build environment. Share portable protocol types and codecs; do not link browser dependencies into the native executable.

## Document and execution

- Code cells contain ordered top-level Rupam items; Markdown cells provide narrative. Store cell identities and layout separately from executable `.rp` source. Save accepted items as an ordinary formatted program; preserve unsaved drafts separately. Reopening must not replay effects.
- Resolve dependencies by binding identity in source order, preserving lexical shadowing. Moving a cell rechecks affected references. Reject unresolved references and dependency cycles.
- Check edits against pinned revisions. Reevaluate only affected pure items; reuse unchanged values and evidence. Coalesce edits, cancel superseded work and publish results atomically. Retain the last good output with a visible stale marker; block dependent execution when prerequisites fail. Reads follow input-revision rules; random, clock and write effects require explicit submission under the session effect policy.

## Interaction

Bare expressions display automatically and `!` taps work as in the REPL;
explicit notebook items are document content, unlike transient REPL input.
Presentation annotations are stored apart from the `.rp` source. Viewer
sorting and filtering are presentation only. Save, lock, unlock, refresh and
revision restore are session operations.

Use session completion, diagnostics, fixes and decisions at source spans. Display typed values and virtualised tables; expose existing viewer summaries, probes, schema, generated code and lock status. Grid transformations edit Rupam source. Fetch bounded Arrow pages by result handle and revision; scrolling performs no analysis. Preserve exact `Int`, optionality, logical metadata and display restrictions. Release result handles when no longer used.

## Local access and distribution

Bind to loopback with an ephemeral port and authenticated browser sessions. Validate request origins and WebSocket handshakes. Give each notebook its own session; agent attachment uses explicit permission and existing exposure rules. Serve assets locally without CDN dependencies. Ship identical frontend assets in native Windows, macOS and Linux releases; the selected R or Python environment remains required.

## Acceptance

Exercise open, edit, inspect, cancel, save, lock, compile and rerun on every supported platform. Compare typed results and diagnostics with the REPL. Verify selective recomputation, failure atomicity, no effect replay, bounded memory, deep scrolling and rejection of stale results. Test native Windows release artifacts on a clean machine. Defer general graphics, modelling, remote hosting, collaborative editing and browser-only execution.
