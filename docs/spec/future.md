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
browser execution backend: the `sql-duckdb` dialect on duckdb-wasm.

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
justify the Core/Async dependency cost, and it builds and runs natively on
Windows; measure on wide grids and live editing.

## Viewer column freezing and large-data summaries

Allow analysts to freeze and unfreeze columns for presentation. The default
freezes visible primary-key columns. A presentation change leaves the
pipeline and its ordering unchanged.

Offer named approximate summaries for large columns, with visible accuracy
information. HyperLogLog is an option for distinct counts and quantile
sketches are an option for quartiles.

## Unicode operators

Unicode spellings of operators, as in Julia: `∈` for `in`, `∉` for
`not in`, `≤`, `≥`, `≠`, `→`, with LaTeX-style `\in`, `\notin` and similar
tab-completion in the REPL and notebook. The formatter keeps the spelling.

## Schema algebra

Row-polymorphism constraints for field presence, absence, replacement,
removal, projection, renaming and composition, so generic transformations,
joins, selector-dependent schemas and `across` expansions infer without
annotations. Precedent: Morris and McKinna, *Abstracting Extensible Data
Types* (POPL 2019).

## Horizontal reductions

`all(columns, predicate)` and `any(columns, predicate)` reduce a predicate
across columns within each row, giving a present `Bool` per row:
`filter(all(bill_len:sex, some))`. `columns` is explicit names, a range or
a predicate selector such as `where(is_number)`, resolved against the schema
at that stage.

Reductions across selected columns within each observation (questionnaire
scores, composite indicators, row-level missingness), with an explicit axis
that fits shape inference and missing policies, preserves the observation
domain and needs no persistent rowwise grouping. Precedents: dplyr
`c_across`, `if_any`, `if_all`; Polars horizontal reductions.

## Symbolic sizes

Symbolic relationships between collection sizes: preservation, equality,
bounds and additive cardinality, for vector compatibility, indexing safety,
reduction preconditions and relational cardinality. They complement
observation-domain identity. Precedent: Futhark's size-dependent types.

## Refinement-carrying predicates

A predicate extracted into a function keeps the refinements of its inline
form, including facts from both outcomes and, where decidable, relations
between values and columns. Precedent: Typed Racket occurrence typing and
latent propositions.

## Inequality and temporal joins

Inequality, interval and as-of joins integrated with nominal key
compatibility, temporal types, cardinality guarantees and evidence. Direction,
tolerance, ambiguity and coverage are explicit; established facts such as
non-overlapping intervals inform cardinality. Precedents: Polars non-equi and
as-of joins; dplyr inequality, rolling and overlap joins.

## Data-discovered enums

`as_enum(x)` (alias `as_factor`) converts a String, Int or Bool column to an
enum whose levels are its distinct present values in ascending order; `None`
stays `None`. Levels are discovered from the snapshot as pivot levels are:
provisional while exploring, committed by `:lock`, and a changed level set
requires relocking. More than 50 levels is the warning `many-levels`. This
needs enum columns on every target (R factors, Polars `Enum`, Arrow
dictionaries) and session-level discovery, so it follows the MVP.

## Help search

`??term` searches names and descriptions of functions, verbs, bindings,
columns and types, from the same declarations as `?name`.

## Repeat

`repeat(xs, times)` repeats the whole vector `times` times and
`repeat_each(xs, times)` repeats each element, with `times` either one Int or
one Int per element. Two functions replace the modes of R's `rep()`: there is
no `length.out`, no recycling of `times`, and no meaning that changes with
argument length. See tidyverse design, "Case study: rep()".

## Comment-preserving formatting

The parser keeps comments as trivia attached to the nearest item or pipeline
stage, so `rupam fmt`, `:save`, pasted drafts and Ctrl+G edits reformat code
without losing them. Until then, drafts containing comments are left as
written.

## Test coverage

Measure line and branch coverage of the compiler, native runtime and console
with `bisect_ppx`, instrumenting libraries through dune's
`(instrumentation (backend bisect_ppx))` and reporting from CI to Codecov.
Deferred: the released `bisect_ppx` 2.8.3 requires `cmdliner` < 2.0 and
`ppxlib` < 0.36, which conflict with the pinned `cmdliner` 2.1.2. Adopt it once
a release supports current `cmdliner` and `ppxlib`; coverage must not change
which tests run or how they pass on any platform.
