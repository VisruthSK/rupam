# Tables

A table has named, typed columns in a fixed schema order and, by default,
unordered rows. Schema order is deterministic and drives selector ranges,
printing and written files, but it is not part of the table's type: table
types, like record types, compare by column name. Rows form a bag (duplicates are meaningful). Its type lists
columns, refinements, keys, row-domain identity, order, and whether the schema
is established or inferred. Columns inherit the domain and order of their
rows; these properties are inferred, not separate user-visible containers.

`Table([{id = 1, site = "A"}, {id = 2, site = "B"}])` constructs a table
from a vector of records, with inferred common field types. An empty vector
requires a row-type annotation. Field names must be unique. Construction
produces an unordered table; it establishes a schema, not key uniqueness.

## Verbs

| Verb | Result |
| --- | --- |
| `mutate(name = e, ...)` | adds or replaces columns in order; each binding sees earlier ones |
| `filter(predicate)` | rows selected by the predicate and missing policy |
| `select(a, b, new = old)` | selected columns, in order, renamed as given |
| `rename(new = old)` | table with columns renamed in place |
| `arrange(a, desc(b))` | rows sorted lexicographically; establishes order |
| `slice_head(n = 5)` | first rows, requiring total order |
| `slice_min(x, n = 5)`, `slice_max(x, n = 5)` | extreme rows with explicit tie semantics |
| `summarize(name = e, ..., by = keys)` | one row per nonempty group: keys, then summaries; one row when ungrouped |
| `count(a, b)` | `summarize(n = n(), by = [a, b])` |
| `distinct(a, b)` | only the selected key columns, one row per distinct combination |
| `left_join`, `inner_join`, `right_join`, `full_join`, `semi_join`, `anti_join` | joins |
| `pivot_longer(cols = [a, b], names_to = k, values_to = v)` | columns to key/value rows |
| `pivot_wider(names_from = k, values_from = v, levels = [a, b])` | key/value rows to columns |
| `bind_rows(t, ...)` | concatenate tables by column name |
| `expand(a, b)` | all combinations of supplied or observed domains |
| `complete(a, b)` | retain observations and add absent combinations |
| `validate(checks..., warn_above = r, stop_above = r)` | table after checking rows against thresholds; experimental, not a source of static facts |

- `mutate` takes scalars (broadcast) or aligned columns; `summarize` takes
  one scalar per group; `filter` takes a scalar or aligned Boolean column.
  Materialised vectors cannot be implicitly attached by position, even if
  lengths match. Use a join or a row-aligned expression in `mutate`.
- Grouping exists only in `by =`, evaluated once at verb entry. The verb
  fixes a grouping witness: the group of every row as of entry. Group
  membership, and every fact the checker uses about groups (uniqueness and
  ordering within a group), refer to the witness, not to the current values
  of the grouping columns. Replacing a grouping column inside the same verb
  changes neither the groups nor those facts; uniqueness of the replaced
  column cannot discharge an ordering requirement over the witness groups.
  Group keys are column names or vectors of names; compute new keys in
  `mutate` first.
- `mutate(..., by = keys)` evaluates reductions per group and keeps every row,
  including missing-key rows as specified in [Missing values](missing.md).
- A summary's expressions see the input columns, not other summary results.
  Compute dependent summaries in a subsequent `mutate`.
- `missing =` is accepted by verbs with applicable decision points. Positional
  operations use [Order](#order). Grouped verbs also accept `group_missing =`
  (see [Missing values](missing.md)). `validate` evaluates each predicate and
  reports counts of TRUE (passed), FALSE (failed) and None (unknown), before
  applying a policy. Under `skip`, the failure fraction is failed / (passed +
  failed); under `keep` it is `None` if any predicate is unknown, otherwise
  failed / total. Under `error`, any unknown predicate fails validation before
  thresholds are applied. A zero denominator gives `None` (no evidence), not
  zero failures; report the undefined fraction and counts without applying
  numeric thresholds. A defined fraction warns above `warn_above` and fails
  above `stop_above` (thresholds in [0, 1]). It never filters rows or establishes
  static facts.
- Duplicate schema or output names are errors. A repeated selection of the
  same column is deduplicated in first-selection order unless explicitly
  renamed. Zero-column tables retain their row count. A selector that matches
  no columns is legal in `select` or `across`, but a pivot needs at least one.

## Column names

Bare named arguments matching a verb's configuration parameters configure
that verb. Backtick-quoted named arguments always declare output columns in
verbs that accept named outputs (`mutate`, `summarize`, `select`, `rename`):

```text
data |> mutate(`missing` = none(x), `by` = region, by = region)
```

Quoting distinguishes the output namespace from configuration, even when the
quoted name is an otherwise valid identifier. The parser retains this
distinction until argument classification. Quoted output names follow the
same sequential evaluation, name collision and schema rules as other output
names; `select` and `rename` require a source column on the right. A quoted
and an unquoted output with the same decoded name are duplicates. A quoted
output may coexist with a bare configuration of the same name. Configuration
uses bare `by`, `missing`, `group_missing` and `order_by`, never dotted
variants. Ordinary input references use `$name` when disambiguation is needed.
Outside named-output positions, quoting only escapes an identifier and does
not introduce a separate namespace. Forwarding preserves whether a named
argument is an output declaration or configuration.

- Inside a verb's arguments, a bare name resolves against the current table
  schema and lexical bindings. If both exist it is an error; `$name` selects
  the column and `^name` the binding. Outside a table-expression context a
  bare name is a binding.
- Inferred table-function parameters distinguish column expressions, key
  names, selectors and ordinary values. At the call site, a column-expression
  argument is checked against the schema of the piped (or explicit table)
  argument. It can be an arbitrary well-typed expression, not just a name:

```text
let mean_by(data, value, key) = data |> summarize(average = mean(value), by = key)

sales |> mean_by(revenue, region)
sales |> mean_by(revenue - cost, region)
```

The TransformIR substitutes column-expression arguments hygienically into the
stages that consume them. Column references resolve by name against the
schema of that stage; lexical bindings retain their captured values. This
is expression substitution, not evaluation to a vector at the call site.
For example:

```text
let mean_active(data, value) =
  data |> filter(active) |> summarize(average = mean(value))

sales |> mean_active(revenue - cost)
```

Only active rows contribute. If a preceding stage replaces `revenue`, the
expression sees the replacement; a removed or incompatible referenced column
is a type error at that stage. The checker rechecks substituted expressions,
including their refinements, grouping, order and missing policies. Key-name
and selector parameters must satisfy their narrower contextual constraints.
Substitution through a join or reshape is allowed only when the referenced
columns resolve unambiguously in the descendant schema; it never aligns
independently evaluated vectors. The table argument evaluates once; deferred
expressions are pure and repeated computation may be shared within a stage.

Row-domain identity is an internal type property. Aliases, projection and
row-preserving mutation retain alignment; filtering changes the domain and
sorting changes its permutation. Joins, summaries, pivots and row binding
establish new domains. Independently materialised tables do not implicitly
combine their columns, even when related or equal in length. Use an explicit
join. The compiler uses alignment facts internally for expression evaluation.

## Selectors and across

Names, ranges (`region:cost`), exclusions (`-cost`), `starts_with(s)`,
`ends_with(s)`, `contains(s)`, `matches(r"...")` and `where(is_number)` resolve
against the schema during checking. Positive selections preserve the stated
order; predicate selections preserve schema order. An initial exclusion
starts from all columns. `is_number` tests the built-in numeric constraint.

`across(selector, f)` in `mutate` or `summarize` expands to one checked
expression `f(column)` per selected column. `across(selector, (mean, sd),
names = "{col}_{fn}")` expands in column then function order. A single function
in `mutate` keeps the column name by default; multiple functions and summaries
use `{col}_{fn}` (anonymous functions need an explicit name). `names =` is a
static template with `{col}` and `{fn}`. Selections and names resolve against
the schema at entry to that `across` call. Implicit selections (predicates,
patterns and ranges) exclude the verb's grouping columns; explicitly named
key columns may be selected in `mutate` and then follow the fixed entry-group
rule. `summarize` cannot overwrite a grouping output. All expressions expanded
by one `across` read the same entry-stage values and schema; compute them
simultaneously, then install the outputs in expansion order. Subsequent verb
arguments see the new columns; selectors are not re-evaluated between generated
expressions. Accidental output-name collisions are errors. Expansion uses the
verb's grouping and missing policies and the function's inferred lifting.
No callback is needed per row.

## Keys and joins

The checker distinguishes a required unique key, uniqueness among present
keys, and uniqueness of key combinations including missing components. A primary
key is required and unique. Grouping and distinctness use exact typed
equivalence, including signed zero equivalence and componentwise missing
identity. Float keys use exact finite binary64 values, never `~=`; keys must
have a supported scalar relational equivalence. Missing join-key components
never match. Composite keys are vectors of components.

Keys come from contracts, `distinct(k)`, `summarize(by = k)`, and
`assert_key(t, k)`, which establishes required uniqueness after checking the
locked data. Grouping or distinctness of optional keys does not establish a
nonmissing primary key. `unique` in a contract establishes uniqueness among
present values only.

- `filter`, `arrange`, and `slice_head` preserve keys. `select` preserves a
  key if every component survives, updating names for renamed selections;
  `rename` updates key names. `mutate`
  preserves keys not replaced and can preserve direct copies of key
  components. Replacing a component invalidates the key unless a known
  injective transformation is proved (e.g. exact checked integer translation).
  Do not assume injectivity of floating-point arithmetic or user functions.
- Removing or replacing columns also removes their refinements and metadata
  unless the new expression establishes them. `summarize` and `distinct`
  establish grouping uniqueness; `skip` on grouping keys also establishes
  their presence in the output. `bind_rows` and reshaping conservatively
  discard input keys unless uniqueness follows from their result semantics.
Key transfer, for a key `k` of the input:

| Operation | Output key |
| --- | --- |
| `filter`, `arrange`, `slice_*` | `k` |
| `select`, `rename` | `k` under its new names, if every component survives |
| `mutate` | `k`, unless a component is replaced by an expression not proved injective |
| `summarize(by = g)`, `distinct(g)`, `count(g)` | `g` only (present under `skip`) |
| joins | the probe side's `k` when the other side's key is proved unique; otherwise none |
| `bind_rows`, pivots, `expand`, `complete` | none, unless established by the result semantics |

`lookup(t, k = v)` returns the row of `t` whose key `k` equals `v`, as a
record `{...}?`, and `None` when there is none. It requires `k` to be a known
key of `t`.

- Join keys compare identical underlying nominal types (or identical primitive
  types); optionality and refinements may differ. No numeric key promotion.

```text
orders |> left_join(customers, by = customer_id)
orders |> left_join(customers, by = (customer_id == id), unmatched = error)
```

- `by` is required: a name, a left/right equality, or a vector of these.
  Equalities here specify names, not ordinary numeric comparisons.
- A join must have evidence for its cardinality or state `relationship =`.
  Known right-key uniqueness suffices for left, inner and filtering joins;
  known left-key uniqueness suffices for right joins; full joins need both
  or a declaration. A semi/anti join never multiplies rows, so it needs no
  relationship declaration. Explicit relationships constrain matching rows:

| Relationship | Matches per left row | Matches per right row |
| --- | --- | --- |
| `one_to_one` | at most one | at most one |
| `many_to_one` | at most one | unrestricted |
| `one_to_many` | unrestricted | at most one |
| `many_to_many` | unrestricted | unrestricted |

- `unmatched = allow` is the default; `unmatched = error` requires match
  coverage of the left side for left joins, right side for right joins, and
  both sides for inner/full joins. It is unavailable for semi/anti joins.
  This differs from dplyr's dropped-row check. Missing keys are unmatched;
  a validated required foreign key may discharge the coverage obligation.
- Equality joins retain left columns followed by right non-key columns.
  Corresponding right keys are omitted; right/full joins coalesce matching
  key columns into the left key names, including right-only rows. A key's
  resulting type reflects the possible contributing source keys (left only
  for left/inner joins, right only for right joins, either for full joins);
  do not make a required coalesced key optional solely due to unmatched
  non-key columns. `keep = both` retains the original keys separately and
  does not coalesce; right key names must not collide (rename first).
- Columns from an unmatched side become optional; duplicate non-key names
  are errors, including self-joins. Rename explicitly. Semi/anti joins retain
  only the left schema and keys.
- Left/inner joins preserve left keys if each left row matches at most one
  right row; right/inner joins preserve right keys under the converse
  condition, with renamed key components as needed. Full joins conservatively
  drop input keys unless required coalesced keys are known unique on both
  sides and nonmissing. Output order is unordered for all joins.

## Reshaping and combining

- `pivot_longer` requires a common compatible type for selected columns;
  numeric promotion and optional lifting apply. The key-name column is
  `String`, the value column has the common type, and remaining columns
  retain their schema order. Output names must be distinct.
- `pivot_wider(names_from = k, values_from = v, id_cols = ids, levels = ...,
  values_fn = f, values_fill = e)` uses remaining columns as `id_cols` by
  default. Levels are explicit static strings or enum values, or inferred
  from the declared enum domain. `names_from` is String or enum; restricted
  values cannot supply generated column names, even if levels are specified.
  If neither is supplied, exploration discovers
  distinct observed names from the complete pure input at that stage, ordered
  by byte order (not physical observation order). The discovered schema is
  provisional. `:lock` or `rupam lock FILE` explicitly commits the discovered
  levels to the analysis entry; no manual source edit is needed. Show the
  proposed names and schema before commitment. A later schema change requires
  explicit relocking and downstream rechecking; a target-dependent or
  effect-dependent discovery cannot be committed as a portable schema. Supply
  explicit levels in that case. Duplicate levels, generated-name collisions,
  missing names and values outside levels are errors (missing names can be
  explicitly excluded with `missing = skip`). Each id/level cell has at most
  one observation unless `values_fn` explicitly supplies a scalar reduction.
  Absent cells are `None` unless a compatible `values_fill` is supplied;
  observed missing cells are unchanged. Output columns follow level order.
  Output is unique on the id combination (required key only if nonoptional).
- `bind_rows` uses the union of names in first-appearance order. Common types
  are inferred per column; absent columns become optional. Incompatible
  nominal types are errors. It does not establish order.
- `expand(a, b)` returns the Cartesian product of distinct values of those
  columns, or specified domains (`year = 2020..=2026`). Known enums supply all
  declared values. `nesting(a, b)` supplies only observed combinations as one domain
  in a product. The result has only domain columns and grouping uniqueness.
- `complete(a, b, fill = {cases = 0}, explicit = new_only)` retains all input
  rows and adds combinations from `expand` that have no matching row. New
  non-domain cells are optional unless filled. Fill may target non-domain
  columns only. `explicit = all` also fills
  pre-existing missing cells; default `new_only` fills only new cells. Fill
  expressions are compatible scalar values. Missing domain values require a
  policy; `keep` includes missing as a distinct level and uses exact
  grouping equivalence for completion (not join matching). `skip` excludes
  missing values from generated domains, not from the retained source.
  Both verbs support local `by =` and return unordered tables.
- Inferred-schema provenance survives transformations, including joins and
  row binding; an explicit source schema, contract, or explicitly committed
  full-snapshot schema clears the corresponding provisional provenance. A
  committed pivot schema does not by itself establish its source schemas.

## Reading and writing

```text
use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
let visits = read_csv("visits.csv", schema = {id: PatientId, site: String, age: Int?})
let raw = read_parquet("raw.parquet")
write_parquet(summary, "summary.parquet")
write_csv(summary, "summary.csv")
```

- `load` reads a contracted table ([Contracts](contracts.md)). `schema =`
  checks exactly the named columns against the given schema; schema order
  determines output order. Extra, duplicate or absent columns are errors.
- Without `schema =` the schema is inferred and the table is marked inferred.
  Inference examines the full locked source, not only a sample. Inferred
  tables may be explored; strict export/run and external writes require an
  established schema. An explicit lock can commit a fully discovered source
  schema, with its parsing rules and exact snapshot; otherwise supply a schema
  or contract. `rupam schema` prints a reusable schema declaration.
- CSV is UTF-8 with a header, comma separators and doubled quotes. Unquoted
  empty fields are missing; quoted empty strings are present strings. No
  implicit trimming or other missing tokens; `missing_tokens = [...]` may
  explicitly add tokens. `TRUE` and `FALSE` parse as booleans, not `T` or `F`.
  Infer the narrowest common supported primitive type, preserving leading-zero
  identifier text as strings; ambiguous types default to String. Explicit
  schemas parse exactly, including nominal wrappers, and reject malformed
  values rather than making them missing. Read raw text to clean with parsers.
- Read integers exactly (no intermediate floating-point parsing). Reading
  converts NaN to `None` and rejects infinity; requiredness and refinements
  are checked after normalisation. Unrepresentable values fail.
- CSV writing emits `None` as an unquoted empty field and present empty String
  as `""`; quote and double embedded quotes as needed to preserve content.
  Numeric output uses a round-trip representation for the stored finite value;
  a schema is still required to restore nominal types. If a zero-column table
  cannot be represented in a host file format with its row count, reject that
  write rather than lose rows.
- Reads use locked snapshots; writes do not modify any locked input. Output
  columns marked restricted are excluded. Writing unordered rows does not
  establish semantic order. `row_id = name` on a reader with defined source
  sequence explicitly captures one-based source position as a required unique
  Int column; this does not implicitly sort the table.

## Order

Ordering is part of the inferred row-domain type, inherited by aligned
columns. Three states suffice: unordered observations, sorted observations
with possible ties (a total preorder), and totally ordered observations.
A sorted table is totally ordered if its sort key combination is known unique; exact
row duplicates still count as separate observations. An unordered bag is not
a set, and a sortable element type does not imply ordered observations.
General partial-order data structures are not needed for these verbs.

- `arrange(a, desc(b))` establishes a lexicographic order; strings sort by
  UTF-8 byte order, enums by declaration, missing values last in either
  direction. Ties carry no implied physical order. A key contained in the
  sort key combination proves totality; other uniqueness may be established on locked
  data. Do not create hidden tie-breakers or automatically sort tables.
- `lag`, `lead`, cumulative functions, `first`, `last`, `slice_head` and
  `ntile` require total order for positional decisions (unless their input
  is an intrinsically ordered vector). `ntile` uses value order first; only
  ties need a total observation order. A sorted input with unresolved ties is insufficient;
  supply meaningful tie-breakers. `order_by =` on a positional operation or
  enclosing verb establishes local lexicographic order; local results map
  back to the original row domain. The requirement is within each group, so
  uniqueness of `(group keys, order keys)` suffices. Unknown uniqueness is
  checked on the locked data, never resolved arbitrarily.
- `slice_min(x, n = 5, ties = keep)` and `slice_max` keep all rows tied at
  the cutoff; `ties = drop` requires tie-breaking only when a tie crosses the
  cutoff. Use established order or `order_by` to distinguish the selected
  rows within that tie block; a proof that no tie crosses the cutoff suffices.
  A selected tie block need not have a deterministic internal sequence; output
  is sorted with ties unless the full output order is proved total. Optional
  selection values require a
  policy: `skip` excludes them, `keep` sorts them last as one tie block,
  `error` rejects them. Results are sorted by the selection order. `n` is a nonnegative Int.
- Filtering, projection, renaming, row-preserving mutation and head slicing
  preserve established observation order. Removing or changing sort columns
  retains the sequence but not the claim it is sorted by their new values;
  the TransformIR retains the original order witness. Grouped mutation and
  operation-local sorting restore row alignment. Summaries, distinctness,
  joins, pivots, expansion and row binding return unordered rows.
