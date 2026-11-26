# Future

All features below are deferred.

## Models and graphics

`~` is reserved; formula expressions are unsupported until a library
interprets them. Model and graphics libraries resolve formulas and aesthetics against a
table's schema as verbs resolve column names:
`lm(y ~ x + (1 | site), data = visits)`, `aes(x = revenue, colour = region)`.
Each library adds core nodes, dialect lowerings and preview handlers.

## Package export

`export let f(...) = ...` and `rupam package r|python` produce an R package
or Python module callable without rupam. Entry points check incoming columns.
Documentation is generated from types.

## Modules

`import helpers` loads `helpers.rp` from the project.

## Decimal

A `Decimal` type for exact decimal arithmetic.

## Dimensional analysis

Typed physical units in the style of F# units of measure and Unitful.jl:

```rupam
let height = 1.75 m
let weight = 72.0 kg
let bmi = weight / height^2   // Float<kg/m²>
```

Compatible units convert (metres plus centimetres), incompatible dimensions
are rejected (metres plus kilograms), and products and quotients derive
compound dimensions. Transcendental functions take dimensionless arguments.
Units are inferred and keep numeric semantics; they are distinct from nominal
identities and from descriptive contract metadata. The surface syntax above is
illustrative. Until then nominal wrappers need explicit unwrapping for
arithmetic.

## Observational grain

Track duplication and aggregation locality after intentional one-to-many
joins; this is distinct from key/cardinality checking.

## Restricted aggregate release

An explicit, auditable release of approved aggregates derived from restricted
columns. Aggregation alone does not establish disclosure safety. Until a
release policy is defined, these outputs remain restricted.

## Dialects

Each dialect connects through the Arrow boundary and its own conformance;
numerical behaviour is never assumed identical.

- `r-datatable`: first through dtplyr over the dplyr lowering, then a native
  emitter only if conformance requires it.
- `python-ibis`: emits an unbound Ibis expression the analyst binds to a
  backend. Evidence records the executing engine; an unbound expression does
  not certify every backend.
- `julia-tidier`, after Arrow logical-type preservation is established.
- webR: browser execution of the `r-dplyr` dialect, subject to Arrow, file
  I/O, package availability and numeric conformance.

## Notebook

A reactive notebook (`rupam notebook`) in the spirit of marimo, specified in
[Notebook](notebook.md).

## Portable compiler

The parser, checker, formatter and lowering as a library free of OS-specific
ingestion and workers, compilable to JavaScript or Wasm with supplied schemas
and evidence. A browser-hosted notebook additionally needs a conformant
browser execution backend.

## Ingestion engine

Talon (raven-ml) is evaluated as a replacement for DuckDB in ingestion, schema
inspection, validation scans and Arrow access. It is adopted only if it
matches on native Windows builds, full-file CSV/TSV inference, Parquet, Arrow
IPC and memory mapping, exact 64-bit integers, finite floats, structured
errors and buffer lifetimes, and wins on cold and warm ingestion, scans, peak
memory and packaging. DuckDB stays until then.

## Optimiser validation

Each TransformIR rewrite of the logical optimiser emits a certificate: rule,
original and replacement subtrees, scope and the facts authorising it. A small
independent validator checks it before lowering, as in Alive2-style
translation validation; an invalid or unsupported certificate keeps the
original expression. Certificates complement Lean proofs and differential
conformance.

## Inference and resolution

Studied against conformance cases, with no rewrite implied:
OutsideIn(X) (implication constraints and local assumptions, for refinements
and policy obligations), Inferno (as reusable inference and elaboration
infrastructure, adopted only if it simplifies rollback, open rows and
diagnostics) and scope graphs (one resolution model for checking, completion
and renaming).

## Environment setup

`rupam env` creates the uv or renv files of a project.

## Test blocks

Named `test` blocks with inline tables, run on every dialect by `rupam test`.

## Inferred refinements

Strengthen the internal refinement engine without new syntax, in the spirit of
Liquid Types (Rondon, Kawaguchi and Jhala, PLDI 2008): Hindley–Milner inference
plus predicate abstraction over a small decidable domain. After
`filter(age >= 18 and age <= 65)`, `mutate(remaining = 100 - age)` gives
`remaining` the interval 35 to 82 without annotations.

| Analysis | Establishes |
| --- | --- |
| intervals | lower and upper bounds through arithmetic, filters and guards |
| cardinality bounds | enough observations, such as two present values for `sd` |
| congruence | divisibility and integral properties |
| relations | facts between values, such as `start <= end` |
| implication | whether known facts discharge a new obligation |

Facts follow Rupam's exact Int and IEEE binary64 semantics, including
rounding, underflow and overflow; real-number reasoning alone is unsound. A
general SMT solver comes only after the decidable domain proves insufficient.
Users never write logical predicates in ordinary annotations. Related work:
Abstract Refinement Types (ESOP 2013), Bounded Refinement Types (ICFP 2015).

## Logical optimiser

A pass over the TransformIR, after checking and before any lowering, that
rewrites table pipelines using facts only the checker has. Every rewrite
preserves the results, diagnostics, evaluation scope and effects defined in
[Guarantees](guarantees.md), and the source map still points each generated
stage at the Rupam stage it came from.

- **Projection pruning.** Drop columns no later stage, output or restriction
  needs, including through joins and pivots, so readers request fewer columns.
- **Predicate pushdown.** Move a filter before a mutate it does not depend on,
  into both sides of a join when its columns allow, and into a reader as a
  row-group filter.
- **Proved-work elimination.** Remove sorts whose order is already established,
  `distinct` over a proved key, `assert_key` on a proved key, missing-value
  handling on columns proved present, and conversions proved exact.
- **Common work.** Share repeated pure subexpressions and identical pipeline
  prefixes between bindings, and fuse adjacent mutates and selects.
- **Join planning.** Choose join order and side by key facts and known
  cardinality without changing the result's row multiset or order guarantees.
- **Reduction rewriting.** Combine reductions over the same group into one
  grouped pass, and turn a filter followed by a count into a conditional sum.

Each rewrite has a named rule, a conformance fixture showing the result is
unchanged, and an entry in `:explain` so the analyst can see which rewrites
applied. Generated code stays readable: an optimised pipeline is still one
idiomatic pipeline per binding.

## Join coverage

Independent coverage requirements for each side of a join, for example
requiring every left row to match while allowing unmatched right rows.

## Typed effects

Rupam restricts I/O, randomness and clock access to the top level. An inferred
effect system could permit effectful user-defined functions while preserving
purity wherever it is required, including lifting, snapshot proofs and
speculative evaluation. Effects would propagate through function composition
without annotations. Introduce this only when concrete analytical use cases
justify relaxing the restriction.

## Recursion

Recursive functions, compiled as host functions without vectorisation.

## Recoverable loading

Reusable loading functions may return `Result<T, LoadError>`, so a caller can
handle a missing or malformed file as a value instead of stopping the
analysis. `LoadError` is an enum of the failure kinds.

## Sources

| Work | Adopted | Deferred |
| --- | --- | --- |
| PingPong | `lookup` on a known key returning an optional row; explicit key transfer rules ([Tables](tables.md#keys-and-joins)) | |
| Quone | | a separately marked interoperability layer for typed foreign functions outside strict mode; `Result` for loading functions |
| PRIMUS | contextual requirements as coeffects ([Types](types.md#contextual-requirements)) | basing new guarantees on empirical studies of common analysis errors |
| Petříček, *Data Exploration through Dot-driven Development* (2017) | | completion that offers only the transformations valid for a table's data-derived type, without exposing the type machinery |

## Bonsai terminal frontend

Evaluate replacing the mosaic REPL and viewer with Bonsai (`bonsai_term`),
sharing components with the Bonsai notebook. Adopt it if incremental
rendering measurably cuts frame time and the frontend code shrinks enough to
justify the Core/Async dependency cost; measure on wide grids and live editing.
