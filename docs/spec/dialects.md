# Dialects

A dialect is a host language and table library.

| Dialect | Host | Libraries |
| --- | --- | --- |
| `r-dplyr` | R | dplyr, tidyr, bit64, arrow |
| `python-polars` | Python | polars, pyarrow |

## Compilation

```text
source -> syntax tree with spans -> resolved names -> inferred types, shapes and refinements
       -> typed core -> checked locked analysis -> host syntax tree -> script, source map, manifest
```

The reference evaluator interprets the typed core and defines expected results
for [conformance](conformance.md). It is not the production runtime. The
compiler checks the locked analysis in the target worker; statically proved
obligations need no execution. Data-dependent obligations may be discharged
by evaluating the relevant pure computation on the locked snapshot, without
committing output effects. Cache proofs against the exact core, inputs and
host environment. Do not treat a preview sample as a proof. Snapshot domain
and schema evidence requires full evaluation of the relevant pure stage and
explicit lock acceptance. It does not establish general facts for other
function instantiations. Never reuse a numerical proof from a different
target unless the proof is independent of target semantics.

## Dialect modules

A dialect provides:

- a lowering of every supported core expression and verb to a host syntax tree;
- a capability table listing the constructs it supports;
- its adapters, packages, and readers and writers per file format.

The host language owns worker startup, host syntax printing and source mapping. Table dialects of that language share these facilities. The viewer compiles Rupam expressions through the same selected dialect as the REPL and script compiler.
The manifest is derived from the typed core, lock and capability table.

## Lowering

- Prefer native vectorised operations. User functions inline where useful.
  No per-row callbacks for core table expressions. Ordinary vector
  `map`, `keep`, `reduce` may use host higher-order functions when a native
  vectorised equivalent does not exist.
- Preserve sequential mutation and lexical capture. `if`, `match` and
  Boolean short-circuiting preserve guarded evaluation: an eager host
  conditional must mask operands or use another safe lowering. In a lifted
  branch, inactive elements cannot cause arithmetic or conversion failure;
  reductions keep their specified group scope. Evaluate a branch reduction
  for a group only if at least one element in that group selects that branch;
  compute it on the full group, not just selected rows. Masking a reduction's
  input by branch membership is not a valid lowering.
- Prefer one readable pipeline per table expression. Hygienic intermediate
  bindings are allowed to avoid duplicate work, preserve evaluation order,
  express necessary host stages or improve clarity. Do not generate
  temporaries mechanically.
- A construct without a semantics-preserving lowering is unsupported in
  that dialect. Strict mode checks the common supported core; no silent
  substitution with a host approximation of an unsupported construct.

## Checks and adapters

Discharge obligations statically first, then at analysis/lock time where
values are fixed, then by native host semantics. Emit a runtime check only
when necessary for an unresolved obligation (e.g. random-dependent
arithmetic, foreign returns, or effects not fixed by the lock). Do not repeat
proved domain, key, bounds or schema checks per stage. No build mode changes
language semantics. Use a small adapter only where native semantics differ;
emit it only when used. Keep adapter bodies separate from the analysis.

| Adapter category | Required behavior |
| --- | --- |
| lock verification | verify content and environment before analysis, without rupam |
| numeric representation | exact Int64, checked promotion/overflow, finite floats, guarded domains |
| reductions | undefined statistics and empty/all-missing sums return `None`; exact integer accumulation; explicit policies |
| table semantics | exact join, ordering, pivot, grouping and schema rules where host defaults differ |
| ingestion | exact parsing, optionality and normalisation of NaN; reject infinity |
| display | rupam formatting; omit restricted values |

The compiler may eliminate a value-dependent check only when its proof is
valid for the target lock and all possible unpinned effect outcomes. Host
numerical operations that cannot provide the required checked semantics must
use an adapter or be rejected. No new general dataframe runtime is required.

A numerical obligation established by computing on locked data (a sign,
presence or bound) eliminates its check only if the computation that
established it is deterministic under the locked execution conditions, or
the obligation holds for every outcome the documented numerical tolerance
permits. Floating-point reductions whose result depends on accumulation
order, threading or vectorisation are not deterministic unless the dialect
pins their order. For those, the obligation must hold with a margin larger
than the reduction's error bound; otherwise the check remains. In
`1.0 / sum([1e16, 1.0, -1e16])` the sum is `1.0` or `0.0` depending on order,
so a nonzero divisor observed once does not remove the divisor check.

## Strict mode

Strict mode is the default. A strict program uses the portable intersection
of the designated dialects' capabilities, with no foreign imports or
dialect-specific functions. With `strict = false`, foreign imports and
dialect-specific functions are allowed, recorded in the manifest, and
restrict the program to compatible dialects.

## Effects and portability

`set_seed(seed: Int64)` sets the target's random stream. `runif(n, min = 0.0,
max = 1.0)` returns a vector of uniform Float64 draws; `sample(xs, n,
replacement = without)` samples from an ordered vector. Counts are checked against supported host ranges; seeds are Int64 from 0
through 2_147_483_647; `min <= max`, and without replacement
`n <= length(xs)`. These are top-level effects, with evaluation in source
order. They lower to the target's seeded RNG. Reproducibility is relative to
its locked algorithm, versions, input and call order; R and Python need not
produce the same draws. Randomness and clock effects are allowed in strict
mode without warnings and recorded in the manifest. Clock results vary with
time unless captured as explicit inputs. A preview and compiled script agree
for the same effect inputs, not necessarily for independently drawn values.

Native floating-point algorithms can differ in rounding and reduction order.
Polars, for example, divides a column by a scalar through its reciprocal, so
`x / 3.0` can differ from R in the last place.
The portable semantics define types, domains, structural rules and numerical
operations, not bitwise equality of arbitrary calculations. Small numeric
differences may affect downstream comparisons, selection, ranks or branches.
Do not promise cross-dialect value equality for numerically unstable programs.
Use the documented conformance tolerance for numerical fixtures, and compare
structural and discrete outputs exactly when they are not subject to those
numerical or effect differences.

## Foreign imports

```text
import r "stats" {
  quantile(x: [Float64], probs: Float64): Float64?
}
```

Available with `strict = false`; `import python` is analogous. Declarations
specify scalar/vector/record types and are trusted for the foreign function's
semantics and purity. Validate returned representations and types at the
boundary. No inferred lifting of arbitrary foreign calls into table callbacks;
only a supported native expression lowering may run inside a table verb.
Foreign calls must be pure and deterministic over their explicit inputs;
functions that read hidden external state or have side effects are outside
this import mechanism. Record imports and dependencies in the manifest.

## Generated scripts

- Run without rupam, with the locked target interpreter and dependencies.
- Deterministic generation and formatter-friendly code. The lock verification
  preamble is separate from the analysis; no repeated successful data-dict
  validation or defensive checks proven redundant on the locked inputs.
- Accompanied by a source map to `.rp` spans and JSON manifest of lock digest,
  compiler version, dialect, packages, discharged obligations, remaining
  checks, and non-portable calls.

## Data interchange

Every dialect reads and writes tables through Arrow: CSV, TSV and Parquet are
parsed by the Arrow readers, so parsing and types agree by construction, and
a runtime can be replaced by any Arrow-speaking one. R and Python run in
separate worker processes with their own sessions. A table passed between
processes goes through an Arrow IPC file that both sides memory-map, so it is
shared without being copied or parsed.
