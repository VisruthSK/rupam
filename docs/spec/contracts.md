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
| `primary_key` | required unique table key |
| `unique` | unique among present values |
| `foreign_key`, relationships | validated join information and nominal domains where unambiguous |
| `values` | nominal enum type for the column |
| `assert` | checked during validation; only entailed facts become refinements |
| `display: restricted` | excluded from previews, viewer, diagnostics and written output |
| `range`, `examples`, descriptions, units | shown on hover; not enforced |
| `number(...)` roles | documentation; the physical type comes from the file |

A column without a declared type is not usable with a static type.

## Physical and nominal types

The file's schema determines `Int64` versus `Float64` and date-time zones. A
physical type without an exact rupam equivalent is a type error for that
column. Requiredness and refinements describe the normalised rupam values.

Each unambiguous single-column entity primary key of primitive identifier
type (`Int64` or `String`) has a nominal domain identified by its contract and table/key identity.
Validated foreign keys to it share that domain. Do not infer equivalence
from names, matching representations or arbitrary relationships. Composite
keys retain tuple uniqueness; do not assign entity identity to individual
components unless explicitly declared or independently established.

Each `values` declaration supplies a nominal enum with stable identity from
the contract and column. Stored values, not display labels, are the
representation. Declaration order defines comparison. Valid constructor
identifiers may be used directly; other values use backtick-quoted constructor
names (or `parse_enum` on the stored value). Constructor collisions are
errors. Equal label lists do not establish shared enum identity.

A rupam-side annotation may share or override inferred nominal domains:

```text
type CustomerId = new Int64
type Status = | Active | Inactive
let customers = load(shop.customers, types = {id: CustomerId, status: Status})
let orders = load(shop.orders, types = {customer_id: CustomerId})
```

`types =` is a partial record type literal mapping columns to declared nominal
types. It may not change the physical representation, requiredness, or
validated value domain. Declared enums must cover exactly the stored values
(by constructor name unless a stored-value mapping is supplied in the type
declaration). An override of a primary/foreign-key domain applies to the entire
unambiguous relationship domain throughout the analysis; conflicting
annotations are errors. A checked mapping between
distinct enum domains is explicit. No additional data-dict keys are needed.

## Validation

- Checking needs the resolved contract and locked source schema. Validation
  reads the whole source, never a preview sample.
- `validate-meta` and `validate-data` must succeed; an incomplete, skipped
  or unsupported step is a failure. Then establish rupam's physical invariants
  on the same snapshot after normalising NaN to `None` and rejecting infinity.
  In particular, a required float containing NaN cannot pass.
- Extract only facts entailed for all relevant present values. A direct
  `population > 0` assertion establishes sign, not presence: data-dict
  assertions can pass on an unknown result. Conditional or aggregate
  assertions do not imply unconditional sign without a valid proof.
- Cache success against the resolved contract, source content, normalisation
  rules and validator/environment versions. Validation and execution use the
  same immutable content. A changed input invalidates the lock; relock and
  reanalyse before compilation. Generated scripts verify the lock instead of
  repeating successful contract validation (see [Projects](projects.md#locks)).
- `display: restricted` follows renames, copies and derived values, including
  aggregates and keys. Do not expose restricted values in error excerpts,
  profiles, previews or output files. This is accidental-disclosure protection,
  not a formal information-flow or confidentiality guarantee. Restricted
  aggregates remain restricted by default. An explicit audited release policy
  for approved aggregates is deferred; no implicit declassification based on
  group size or aggregation.

## Definitions

data-dict `definitions` whose expressions translate exactly are available as
rupam functions. Other definitions are a type error when used.
