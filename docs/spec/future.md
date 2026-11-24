# Future

All features below are deferred.

## Models and graphics

`~` builds formula values, which are type errors until a library interprets
them. Model and graphics libraries resolve formulas and aesthetics against a
table's schema as verbs resolve column names:
`lm(y ~ x + (1 | site), data = visits)`, `aes(x = revenue, colour = region)`.
Each library adds core nodes, dialect lowerings and preview handlers.

## Foreign imports

```text
import r "stats" {
  quantile(x: [Float64], probs: Float64): Float64?
}
```

Available with `strict = false`. The declaration is trusted; the returned
value's type is checked at the boundary. Recorded in the manifest; restricts
the program to dialects of that language.

## Package export

`export let f(...) = ...` and `rupam package r|python` produce an R package
or Python module callable without rupam. Entry points check incoming columns.
Documentation is generated from types.

## Modules

`import helpers` loads `helpers.rp` from the project.

## Decimal

A `Decimal` type for exact decimal arithmetic.

## Dialects

`r-datatable`, `r-polars`, `julia-tidier`.

## Environment setup

`rupam env` creates the uv or renv files of a project.

## Recursion

Recursive functions, compiled as host functions without vectorisation.
