# Parallel one-pass ingestion

**Status:** Not started. One pull request after Talon (#5).

## Objective

Reading an input for checking or locking costs what the hardware costs. One
read of each byte produces its BLAKE3 digest, its schema and the evidence its
obligations demand, using every core.

| Input | Target |
|---|---|
| Small files (KB to a few MB) | the cost of opening and reading the file: tens of microseconds, no domain or pool start-up on the path |
| Files in the page cache | 5–10 GB/s on 16 cores, workload dependent; at least 1 GB/s on one core |
| Cold files | the storage's sequential throughput (3.48 GB/s on the reference NVMe) |
| Files larger than memory | the same throughput, in bounded memory |

Throughput is reported against a raw read of the same file on the same
machine, cold and warm (`docs/spec/performance.md`). Semantics do not change:
the grammar, types, first-error-in-file-order diagnostics and line numbers in
`docs/spec/tables.md` stay exactly as they are.

Read first: `docs/spec/tables.md`, `docs/spec/projects.md` (Locks),
`docs/spec/performance.md`, `AGENTS.md`, and Ellmann and Neumann, *One Pass to
Parse Them All: Fused Parallel CSV Processing*, PVLDB 19(11), 2026.

## Where it stands

`Data_file.read` reads a CSV once: `Talon.Scan` tokenizes sequentially and
feeds each read chunk to an incremental BLAKE3 hasher, while three domains
classify finished batches (`Parallel.ordered`). Parquet nullability scans
column chunks on the pool. Measured on `trips.csv` (1.115 GB): 0.47 GB/s warm
in WSL on a quiet machine, and 0.12 GB/s natively on Windows for a 24.5 GB
file. The tokenizer is one core, so it is the ceiling.

## Design

Ingestion is a fold over fixed-size slices with a mergeable accumulator, the
model in the paper: `init`, `accumulate` and `merge`, merged in file order.
Our accumulator is what inference already keeps per column (candidate types,
presence, NaN, first infinity, first error); the lock evidence (counts,
bounds) joins it.

1. **Windows.** Read the file in windows of 64–256 MiB with direct I/O into
   aligned buffers, the reader `Blake3.file` already uses at 2.9–3.4 GB/s,
   two windows in flight so reading overlaps work. Memory stays bounded for
   any file size. Small files take one buffered read and the sequential path.
2. **Slices.** Split each window into 1 MiB slices, handed to a persistent,
   session-owned domain pool through an atomic counter (no per-call pool
   start-up). Each slice is touched by one domain while it is in cache.
3. **Hash and count, per slice.** In one pass over the slice: hash its BLAKE3
   chunks (16 MiB subtrees span 16 slices; merge chaining values in order as
   `Blake3.file` does), count quotes, and count newlines.
4. **Exact boundaries.** Our grammar rejects a quote inside an unquoted field
   and escapes a quote by doubling it, so in every accepted file the quote
   count's parity gives the exact inside-quotes state at each slice start;
   a prefix over slices also gives each slice's first line number. No
   speculation and no retries. A malformed file is still reported correctly:
   every slice before the first malformation is well formed, so its state is
   exact and the first error in file order is found where the sequential
   reader finds it.
5. **Tokenize and classify, per slice.** From its first record boundary to
   the next slice's, build the structural index with SIMD and fold records
   into the slice's accumulator. A record crossing a window edge is carried to
   the next window.
6. **Merge** accumulators in slice order: AND the type candidates, OR
   presence and NaN, keep the earliest infinity and the earliest error with
   its line, and finish the BLAKE3 tree.

### Per-core speed

The parallel structure only multiplies per-core speed; each core must reach at
least 1 GB/s.

- **Structural index in C**, beside the BLAKE3 kernels and chosen at runtime
  the same way (AVX2, AVX-512, NEON, scalar fallback): 64-byte vectors,
  bitmasks for separator, newline, carriage return and quote, the in-quotes
  mask by prefix XOR (carry-less multiply: `pclmulqdq`, `vmull_p64`), field
  begin and end positions extracted from the masks. The paper's core costs
  about 1 cycle per byte. Field counts are checked against the header's.
- **UTF-8 validation** vectorized per slice, as in simdjson; nearly free.
- **Classification** without per-field calls or allocation: an 8-byte
  digit test, fixed-width date checks, skip a column entirely once it can only
  be `String`, and fold counts and bounds as integers.
- No C++; plain C only, as for BLAKE3 and the lz codecs.

### Parquet

Footer statistics settle what they can. Otherwise the same windowed reader
hashes the file and the pool decodes only the column chunks evidence still
needs, from the bytes already read.

## Verification

- The tokenizer agrees with DuckDB field for field, with slice and window
  boundaries forced at every awkward position: inside a quoted field, between
  `\r` and `\n`, between the two quotes of `""`, inside a multi-byte UTF-8
  character, and at a BOM.
- The parallel result equals the sequential result (types, nullability,
  counts, bounds, digest, first error and its line) on every fixture and on
  generated files, property-tested.
- The digest equals `Blake3.file` on the same bytes.
- `scripts/dev check`, the Windows-native suite and CI on every release
  platform.

## Measurement

For each of a KB file, `trips.csv` (1.1 GB) and a 24 GB file, cold and warm,
natively on Windows, Linux and macOS: raw read, digest only, the full read
(digest, schema, evidence), and DuckDB's whole-file inference and parallel
load, alternating in one session. Report GB/s and the fraction of the raw read
for each. First explain why the Windows-native reader is about 4x slower than
WSL on the same code.
