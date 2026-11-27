# Runtime and release

## Arrow boundary

Arrow is the canonical typed interchange and completed-result representation.
Ingestion establishes one interpretation of each pinned input (schema, nulls,
parsing rules, exact values, source identity) from the full snapshot, and the
backend consumes that same interpretation through Arrow-backed inputs or an
equivalent reader. Nominal identities, row domain, keys, order, restrictions
and evidence are Rupam metadata on the immutable result; an Arrow schema does
not establish them.

- Transfer, paging, inspection and backend handoff use Arrow record batches:
  the C Data/Stream interfaces in process, IPC or memory-mapped files across
  processes. Buffer ownership and release are explicit. Batches are bounded
  and inputs may exceed memory.
- Exact signed 64-bit integers, optionality, finite floats, zero-column
  cardinality and order survive every boundary. Intermediate dplyr or Polars
  operations stay native.
- Sharing Arrow buffers does not establish semantic conformance of a dialect.

## Adapters

Generated R and Python are readable, idiomatic host code that computes
values: conversions, missingness, guarded evaluation and order. They contain
no checks of any kind. The same lowering with checks runs inside Rupam's
session during development, where every guarantee is established. Rupam verifies and locks during development. Standalone code
performs no input hashing, lock, contract, environment or snapshot verification.
Supply the locked snapshot and backend when running a native export. Changed
inputs need a fresh Rupam check, lock and export. A format whose standalone reader cannot reproduce canonical ingestion is
exported through a canonical typed input artifact or rejected. Helpers are
emitted only when used; lowering goes through the host ASTs and printers;
process boundaries return structured errors.

## Versions

`rupam --version` and `:version` print one build identity: package version,
source revision with a dirty marker when known, specification and protocol
versions, and platform. `--version --verbose` adds the active target and its
interpreter and package versions. Development builds are
`0.0.0-dev+<commit>[.dirty]`; the first alpha is `0.1.0-alpha.1`
([SemVer](https://semver.org/)). Building does not require Git. A change of
compiler or specification identity invalidates locks.

Alpha is experimental: syntax, semantics, configuration and protocol break
freely, and every file and schema format stays at version 1.

## Supported versions

| Component | Floor | Reason |
| --- | --- | --- |
| dplyr | 1.1.0 | `.by` grouping in generated code |
| tidyr | 1.0.0 | `pivot_wider` |
| arrow (R) | current CRAN release | ingestion and Parquet; requires R ≥ 4.2 |
| bit64 | 4.0 | exact `Int` |
| tzdb | current CRAN release | arrow's time zone database; without it arrow prints a warning on Windows |
| R | 4.2 | arrow's `Depends`; dplyr 1.1.0 alone needs 3.5 |
| Polars | 2.2 | 2.x expression API; the locked development environment |
| pyarrow | 20 | Arrow transfer |
| Python | 3.10 | Polars 2.x; generated code needs 3.8 (`:=`) |
| OCaml | 5.4 | build only |

The R and Python floors are derived, not yet tested; they become supported
once CI passes the oldest combination. Minimum supported, tested and recommended versions are listed separately in
release notes; CI runs the oldest supported combination. Generated-script
requirements are independent of build requirements.

## Distribution

Releases ship native binaries for Windows x86-64, macOS and Linux. Windows
support is established by native Windows tests (terminal, linking, processes,
paths and Unicode, binary I/O, Arrow mapping, file cleanup), not by WSL.
Release artifacts are smoke-tested on clean machines without OCaml, opam, Node
or WSL. R and Python environments are the user's.

## Repository layout

Root configuration exists only for a repository-wide environment. Test
datasets and their Python environments live in the test that owns them;
regenerable artifacts are ignored.
