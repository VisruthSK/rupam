# Contracts

rupam reads [data-dict](https://data-dict.tidyverse.org/) contracts, version
0.1.0, through the data-dict CLI: `export-spec` for the resolved contract,
`validate-meta` and `validate-data` for validation reports. rupam adds no
keys to `data-dict.yaml`.

```text
use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
```

## Declarations

| data-dict | rupam |
| --- | --- |
| `required` | column type `T`; otherwise `T?` |
| `primary_key` | table key; required and unique |
| `unique` | unique among present values |
| `foreign_key`, relationships | join information, established by validation |
| `values` | enum type for the column |
| `assert` | checked during validation; comparisons with zero become sign refinements |
| `display: restricted` | excluded from previews, the viewer, diagnostics and written output |
| `range`, `examples`, descriptions, units | shown on hover; not enforced |
| `number(...)` roles | documentation; the physical type comes from the file |

A column without a declared type is not usable with a static type.

## Physical types

The file's schema determines `Int64` versus `Float64` and date-time zones. A
physical type without an exact rupam equivalent is a type error for that
column.

## Validation

- Checking needs only the contract and the file schema.
- Values are validated before any preview, run, written file or generated
  script reads them, over the whole source.
- An incomplete, skipped or unsupported validation step is a failure.
- A successful validation is cached against a hash of the contract and the
  source files.
- Generated scripts validate their contracted sources first.

## Definitions

data-dict `definitions` whose expressions translate exactly are available as
rupam functions. Other definitions are a type error when used.
