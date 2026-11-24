# Contracts

rupam reads data contracts written in the upstream
[data-dict](https://data-dict.tidyverse.org/) format, currently version
0.1.0. rupam never adds its own keys to `data-dict.yaml` and does not
reimplement data-dict's validator. It calls the data-dict CLI:
`export-spec` for the resolved contract, `validate-meta` for physical types
and `validate-data` for values, and reads their JSON reports.

```text
use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
```

## What the declarations mean

| data-dict | In rupam |
| --- | --- |
| `required` | the column is `T`; otherwise it is `T?` |
| `primary_key` | a key of the table, so it is also required and unique |
| `unique` | unique among present values; it does not make the column required |
| `foreign_key`, relationships | typed join information, proven only by validation |
| `values` (enumeration) | an enum type for the column |
| `assert` | checked by data-dict during validation |
| `display: restricted` | left out of previews, the viewer, diagnostics and written output |
| `range`, `examples`, descriptions, units | documentation shown on hover; never enforced |
| `number(id)`, `number(quantity)`, ... | the column's role; the physical type comes from the file |

A column with no declared type exists but cannot be used with a static type.

## Physical types

The file's own schema, read through DuckDB, fixes `Int64` versus `Float64`
and the time zone of date-times. A physical type rupam cannot represent
exactly is a compile error for that column, not a silent conversion.

## Validation

Type checking needs only the contract and the file's schema. Values are
validated before anything reads them: a preview, `run`, a written file, or the
generated script.

- The whole source is validated, before any `filter` or column selection
  could hide a bad row.
- Validation that is incomplete, skipped or unsupported counts as failed.
- A successful validation is cached against a hash of the contract and the
  files, so an unchanged file is scanned once.
- Generated scripts carry the same preflight unless the project turns it off
  (see [Projects](projects.md)).

## Definitions

data-dict `definitions` (named filters, derived values and metrics) are
imported as rupam functions when their expressions translate exactly into
rupam. A definition data-dict grades as divergent or unsupported is not
imported; using it is a compile error that names it.

A program that type-checks is correct relative to its contract. Validation is
what makes the contract true of the data.
