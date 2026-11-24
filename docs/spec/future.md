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

## Recursion

Recursive functions, compiled as host functions without vectorisation.
