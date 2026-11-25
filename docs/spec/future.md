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

Units and dimensional arithmetic, meaningful nominal numeric derivation,
and compound dimensions. For now nominal wrappers require explicit unwrapping
for arithmetic; metadata units remain descriptive.

## Observational grain

Track duplication and aggregation locality after intentional one-to-many
joins; this is distinct from key/cardinality checking.

## Restricted aggregate release

An explicit, auditable release of approved aggregates derived from restricted
columns. Aggregation alone does not establish disclosure safety. Until a
release policy is defined, these outputs remain restricted.

## Dialects

`r-datatable`, `r-polars`, `julia-tidier`.

## Environment setup

`rupam env` creates the uv or renv files of a project.

## Test blocks

Named `test` blocks with inline tables, run on every dialect by `rupam test`.

## Logical optimiser

A pass over the typed core, after checking and before any lowering, that
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
