# Guarantees

A checked program on its locked inputs has the following properties on
successful execution. Invalid inputs, unresolved requirements and failed
checked operations are errors, not silent fallback values.

## Values

- Every expression has a static type. Compatible numeric types promote
  predictably and exactly; nominal types never implicitly unwrap or promote.
- Vectors are homogeneous and never recycle.
- Missing data is `None`; no value is NaN or infinity. Arithmetic overflow
  fails; IEEE subnormal numbers and underflow to zero are valid.
- Numeric domain requirements are established by sound refinements or accepted
  full-snapshot proofs, with necessary checks for unpinned effects. Only
  selected guarded computations consume guarded operands. Integer arithmetic
  is exact in the portable range.
- `==` and `!=` use exact equality for equatable values, including Float64;
  `~=` is symmetric approximate numerical comparison. Relational keys use
  exact equivalence. Binding a constant does not change equality semantics.
- Rounding is half to even unless `ties = away` is given. `var` and `sd`
  divide by `n - 1`. Undefined statistics, including sums of no present
  observations, are missing.
- Values are immutable; closures capture values. Numeric literals adapt to
  context and default according to their syntax.

## Control flow

- `if` and match guards receive a present Bool per selected element; filters
  can use optional predicates only with a missing policy.
- Boolean operators obey three-valued logic; direct comparisons against
  literal `None` are rejected. Presence tests establish refinements.
- Every `match` is exhaustive. `-x^y` and chained comparisons do not parse.

## Tables and columns

- Every column used exists in its stage's schema; names match exactly.
  A bare name never silently refers to both a column and a binding.
- Optional values at decision points have an established policy. Mutation
  preserves rows; summaries produce one row per nonempty group, or one row
  for an ungrouped summary.
- Column-expression substitution preserves lexical bindings and row-domain
  alignment. Unrelated materialised columns never combine implicitly.
- Joins state keys of compatible nominal types and cannot multiply rows
  without established or explicitly declared cardinality. Missing join keys
  never match. Declared coverage and uniqueness are checked.
- Order is an inferred row-domain property. Positional operations require
  a total order within their group when position matters, not arbitrary physical
  order. Extreme selection needs tie-breaking only at an ambiguous cutoff;
  deterministic membership does not imply a total output sequence. Ties are
  explicit; string ordering is byte order.
- Categories are nominal enums with checked values. Ranges never count down.
  Indexes are present Int64 values from 1; out-of-bounds accesses fail.

## Dates and times

- Date-times in different zones never combine without `to_zone`.
- Every date-time has a zone. `Date` and `DateTime` never compare.
- Month arithmetic never yields an invalid date.

## Execution

- A preview and script agree on the same locked snapshot, target environment
  and effect inputs. Strict mode shares portable core semantics, subject to
  documented native floating-point rounding and random/clock differences.
- Full-snapshot schema commitments and domain proofs apply to their exact
  input and stage, not arbitrary future inputs. A changed snapshot is reanalysed.
- All sources and relevant dependencies are locked; changed inputs require
  reanalysis, relocking and recompilation. Contract and schema obligations
  are checked before relying on their properties.
- Proven obligations emit no redundant checks; necessary checks are never
  disabled. A standalone script verifies its lock before analysis.
