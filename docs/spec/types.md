# Types

## Inference

Types are inferred by Hindley–Milner inference extended with row
polymorphism, built-in constraints, shapes, optional values, row-domain and
order properties, and refinements. Bindings generalise over unsolved type and
shape variables and their constraints; each use instantiates them independently:

```text
let station(note) = note.station      # ({station: a, ..r}) -> a
```

## Inference core

The implementation must define and test a small inference calculus before
extending the standard library. The internal judgment for an expression
records its value type, optionality, shape, row domain, order, refinements,
evaluation scope and outstanding constraints. Function schemes quantify only
variables and constraints not free in the lexical environment. Snapshot
proofs remain tied to their concrete instantiation, not quantified facts.

Parameters have contextual roles: ordinary value, deferred column expression,
key name or selector. Infer roles from consumption sites and propagate role
constraints through calls and higher-order arguments. A deferred expression
carries a resolved lexical environment, column dependencies and a schema
requirement for each consuming stage. It is not a materialised vector. Key
names require a single column reference; selectors require a statically
resolvable selection. A name used both as a key and an expression denotes
that column at each expression use. A parameter requiring both an ordinary
value and a deferred expression is ambiguous: split it into two parameters
or use explicit context annotations where one role suffices for all uses.
A deferred expression may be a pure scalar expression with no column dependencies;
it broadcasts at its consuming stage. Ordinary scalar parameters may broadcast
inside verbs; they are never implicitly reinterpreted as column references.

The following rules constrain elaboration:

1. Resolve lexical references to binding identities and column references to
   stage-relative names. Retain ambiguity errors at the original resolution
   site; substitution never resolves an already-bound lexical reference again.
2. Instantiate function schemes at calls; collect row-field requirements,
   numeric constraints, contextual roles and scope requirements from all uses.
   Solve record rows by field name, with open row variables for unused fields.
3. For a common-type site, unify identical nominal/structural types, lift
   optionality if either operand is optional, and choose Float for mixed
   primitive numbers. Apply recursively to compatible fields. Record promotion
   obligations separately from unification. Unconstrained literals default only
   after contextual constraints are solved; generalise numeric literal constraints
   in reusable functions.
4. Scalar operands broadcast; aligned columns combine only within a common
   domain, vectors require equal length. Reduction signatures consume a
   collection in its enclosing scope and return one value per group; transforms
   return aligned values. Propagate these constraints through function schemes.
   Do not infer a reduction merely because a result is scalar.
5. Elaborate verbs in stage order, applying their schema, key and order rules.
   Resolve selectors and check substituted expressions at every consuming stage.
   Refine facts along guards and filters; conservatively discard invalidated facts.
   Solve lexical policy defaults and then outstanding contextual policy constraints.
6. Reject contradictory constraints. For underdetermined contextual roles or
   result schemas, report the relevant uses and request a type/role annotation
   or explicit schema. Do not choose a meaning based on host evaluation.

Optional parameter annotations `Value<T>`, `Column<T>`, `Key<T>` and
`Selector` describe contextual roles only; they are not runtime containers.
`Column<T>` requires a deferred column expression with element type T at its
consuming stages; `Key<T>` requires a column name with compatible element type.
For a key used as an expression, retain the name role and resolve its value
at the consuming stage. These annotations may appear only on parameters.
The existing type-argument grammar covers their spelling. Ordinary annotations
constrain value types without introducing a new role. At a higher-order call, instantiate the function argument's scheme as for an
ordinary HM use, preserving its contextual constraints in the instantiated
type. The receiving function must supply those requirements. This does not
introduce first-class polymorphism or require rank-N types.

### Contextual requirements

A function type carries the contextual requirements of its body as a
coeffect: evaluation scope (whole vector, group, row), unresolved missing
policies, row-domain alignment and order requirements. These are inferred,
never written. Syntax-directed rules:

1. **Variable.** Instantiating a scheme copies its requirements with fresh
   scope, domain and policy variables.
2. **Abstraction.** Requirements that arise in a function body from its
   parameters become the function's latent requirements. They are not
   requirements of the defining context.
3. **Application.** Applying a function adds its latent requirements to the
   application. A parameter of function type contributes the requirements of
   whatever function is passed for it, so a higher-order function's scheme is
   generic in them, as with effect polymorphism.
4. **Discharge.** A verb discharges requirements against its context: `by =`
   supplies the group scope, `missing =` and lexical defaults supply
   policies, and the stage supplies the row domain and order. A requirement
   still unresolved at a top-level binding or verb stage is a type error at
   that site.

```text
let center(x) = x - mean(x)        # requires: group scope over x's domain
let apply(f, x) = f(x)             # requires: whatever f requires
data |> mutate(z = apply(center, value), by = group)
```

Here `apply(center, value)` inherits `center`'s group-scope requirement,
which `mutate(..., by = group)` discharges, so `mean` runs per group exactly
as in `mutate(z = value - mean(value), by = group)`.

Inference aims for a most general supported constraint scheme, but the full
system makes no unproved principal-type claim. The implementation must publish
syntax-directed rules for this core (including generalisation, role resolution,
common types and staged row operations), specify deterministic ambiguity/defaulting
rules, and test extraction/inlining equivalence for hygienic, well-typed helpers.
Adding columns at a later stage must not capture a lexical reference fixed
at the original resolution site. New ambiguities at a genuinely new resolution
site are errors. The compiler must not depend on backend-specific inference.

## Value types

| Type | Values |
| --- | --- |
| `Bool` | `TRUE`, `FALSE` |
| `Int` | integers from `-9_223_372_036_854_775_807` through `9_223_372_036_854_775_807` |
| `Float` | finite 64-bit floating-point numbers |
| `String` | UTF-8 text |
| `Date`, `Time`, `DateTime<Z>`, `Duration`, `Period` | [Dates and times](#dates-and-times) |
| `Regex` | patterns in the regex dialect ([Standard library](stdlib.md#regex)) |
| `T?` | a `T` or `None` |
| `[T]` | vectors |

| `{a: A, b: B}` | records |
| `A -> B` | functions |
| `Table<{...}>` | [tables](tables.md) |

There is no null, NA, NaN or infinity.

## Constraints

| Constraint | Types |
| --- | --- |
| `number` | `Int`, `Float` |
| `comparable` | numbers, `String`, `Bool`, dates and times of compatible types, enums (declaration order), vectors of comparables (lexicographic), newtypes deriving `comparable` |
| `equatable` | `Int`, `Float`, `Bool`, `String`, dates and times, enums, and vectors, records and variants with equatable components; eligible newtypes |
| `showable` | primitive values, tables and composites of showable values; not functions |

There are no user-defined type classes.

## Shapes and lifting

Every expression has a shape: a scalar, vector, row-aligned column expression,
or table. A column also carries its row-domain identity and inherited row
order (see [Tables](tables.md#order)); an unordered column is a bag of
observations, not a set of distinct values. Element comparability is separate
from observation order. Vectors have an intrinsic position order.

- A function whose body uses only scalar operations lifts element-wise.
- Reduction and transform constraints propagate through calls and higher-order
  functions, separately for each parameter. A consumed collection is the
  entire vector or current group; a scalar reduction broadcasts to that group
  when combined with a column. Nested reductions evaluate inner results first;
  they do not introduce another grouping level. Reducing a scalar is an error
  unless it is a one-value-per-row broadcast expression in a table context.
- Single values broadcast. Vectors of different lengths do not combine; no
  recycling. A materialised vector is not implicitly aligned with table rows.
- Columns from different table stages cannot be combined as materialised
  values. Table-expression parameters instead use the substitution semantics
  in [Tables](tables.md#column-names).
- The TransformIR records evaluation scope, shape, domain, order, required
  constraints and unresolved missing policies. Function generalisation and
  instantiation preserve these properties; no user-visible lifting annotations
  are required. Hover shows the chosen lifting.
- Every conditional construct elaborates to one guarded choice in the typed
  core: `if`, `match`, `let ... else`, `coalesce`, `and` and `or`. A guarded
  choice selects an operand per element for lifted expressions, and once per
  group for scalar conditions. The rules below apply to all of them alike.
  - Only selected element-wise computations run, and only they may fail.
    Inactive operands cannot fail. All operands must type-check under their
    guards, including inactive ones.
  - A reduction inside an operand keeps its full enclosing group scope. It
    runs once per group, on the whole group's input, if any row of that group
    selects the operand, and does not run otherwise.
  - A guard refines the values it tests at the selected rows. It never refines
    the input of a reduction, which spans rows the guard did not select.
  - Nested guarded choices compose.

  For `mutate(result = coalesce(x, mean(y)), by = g)`: in a group where every
  `x` is present, `mean(y)` does not run; in a group with a missing `x`, it
  runs once over all of that group's `y`.

## Optional values

- `T?` is a `T` or `None`; `Some(v)` matches a present value. Nested options
  are flattened. A `T` lifts to `T?` without an explicit constructor.
- A `T?` is accepted as `T` only when presence has been established.
- Arithmetic and ordinary comparisons propagate `None`. Comparing directly
  against literal `None` using `==` or `!=` is a type error: use `none(x)` or
  `some(x)`. These predicates return present `Bool` and refine their operands.
- Boolean operators use three-valued logic: `FALSE and None` is `FALSE`,
  `TRUE or None` is `TRUE`, `TRUE and None` and `FALSE or None` are `None`,
  and `not None` is `None`. `and` and `or` skip an unnecessary right operand
  where the left operand determines the result.
- `a ?? b` and `coalesce(a, b, ...)` are one operation: operands evaluate left
  to right, once, only where every preceding operand is absent; for columns,
  under the unresolved-row mask with group-scoped reductions intact. The
  result has the common payload type and is required when any operand is
  required, so `income ?? 0` is `Int`. All operands are checked statically.
- `match?`, `Some`/`None` patterns and the missing policies also handle absence.
  `if` and match guards require `Bool`; `filter` also accepts `Bool?` with a
  policy. The two branches of a conditional and all match arms infer a common
  type and compatible shape.
- Presence and sign refinements follow successful tests, including conjunctions
  and negated presence tests, in branches and filters. After `filter(some(x))`,
  `x` is present; after `filter(x > 0, missing = skip)` it is present and
  positive. With `keep`, a retained unknown predicate proves neither
  presence nor unconditional sign unless the predicate implies it for every
  retained present value. Replacing a column invalidates its old refinements.

## Numbers

- `Int` arithmetic is exact within the symmetric portable range in the value
  type table (the excluded minimum signed value is reserved by R bit64).
  Out-of-range results fail with `arithmetic-overflow`.
- `Float` uses IEEE 754 binary64, round-to-nearest ties-to-even. Nonfinite
  arithmetic results fail; subnormal results and underflow to signed zero are
  valid. The checker never infers a nonzero result when rounding may erase it.
- Numeric common types follow Julia-style promotion: `Int` with `Int`
  remains `Int`; a mix with `Float` promotes to `Float`. Promotion is
  implicit in arithmetic, numeric comparisons, numeric function arguments,
  compatible record/vector fields and common-type contexts (branches,
  reshaping, row binding). An integer converted to float must be exactly
  representable or fail with `precision-loss`. No implicit conversion from
  floats to integers (except exactly adaptable literals), booleans to numbers,
  or between nominal types. Join keys
  do not promote. Inference chooses the result type independently of values.
- Numeric literals adapt exactly to their inferred context and default to
  their lexical type only when unconstrained. `let inc(x) = x + 1` remains
  polymorphic over numeric inputs. A decimal literal may round to its nearest
  finite binary64 value; adaptation of an integer literal to Float is exact.
- `/` returns `Float`; `div(a, b)` takes integers and truncates toward zero.
  `%` is modulo with the sign of the divisor. Both operands of integer-to-float
  arithmetic use checked promotion. `div` and `%` require nonzero divisors.
- `x ^ y` returns `Float` for all numeric operands. Positive bases admit any
  finite exponent. A zero base admits nonnegative exponents (`0 ^ 0 = 1.0`);
  a negative base requires a whole exponent, including a whole Float.
  Negative integer powers are fractional. Domain requirements must be
  established as below; overflow is an arithmetic error.
- `==` and `!=` always use exact value equality and its negation, including
  Float and named constants. For scientific comparisons with rounding
  tolerance, use `a ~= b` (also `a ≈ b`), which uses
  `abs(a-b) <= max(atol, rtol * max(abs(a), abs(b)))`, with `atol = 0.0` and
  `rtol = sqrt(eps(Float))` (about `1.49e-8`). This is Julia's symmetric
  `isapprox` convention, based on IEEE binary64 machine epsilon `2^-52`, not
  an IEEE-prescribed equality tolerance. `is_approx(a, b, atol = ..., rtol = ...)`
  accepts finite nonnegative tolerances. Zero compares equal to signed zero.
  Relational key equivalence is exact, including for floats (see Tables),
  never approximate. Implement comparisons without overflowing intermediates.
- `round(x, digits = 0, ties = even)` rounds half to even; `ties = away`
  rounds half away from zero. `digits` is Int; negative digits round to
  powers of ten. A nonfinite result fails; signed zero is retained. `prod`
  returns `Float` when defined, otherwise `None`.

### Refinement-aware exhaustiveness

A `match` or `match?` is exhaustive when its arms cover the subject's
established domain, not necessarily its primitive type. Domain facts come from
inferred interval refinements, validated contract bounds, flow refinements of
preceding transformations and admissible snapshot evidence. A function whose
arms do not cover its parameter's type carries the uncovered domain as a
latent requirement in its scheme; each application discharges it from the
argument's facts or is rejected with `nonexhaustive-match` naming the
uncovered interval. There is no new syntax.

```rupam
let elevation(x) = match? x {
  1..<1000 => "Low",
  1000..<1500 => "Medium",
  1500..<2000 => "High",
  2000..=3000 => "Very high",
}
```

`elevation` requires every present argument to lie in `[1, 3000]`.

### Sign and presence refinements

A numeric type may carry `> 0`, `>= 0` or `!= 0` after the type, e.g.
`Float > 0` or `Float? > 0` (every present value is positive).
Integral-valuedness and presence are also inferred. These properties have no
runtime representation. Exact constant and interval facts may be retained
internally to discharge obligations, without adding user-defined refinements.

| Operation | Requires on present operands |
| --- | --- |
| `a / b`, `div(a, b)`, `a % b` | `b != 0` |
| `sqrt(x)` | `x >= 0` |
| `log(x)`, `log2(x)`, `log10(x)` | `x > 0` |
| `x ^ y` | `x > 0`, or `x >= 0` and `y >= 0`, or whole `y` and (`x != 0` or `y >= 0`) |

Sources:

- literals, by value, and constant evaluation;
- sound arithmetic on refined operands: nonnegative sums and products remain
  nonnegative; positive integer products remain positive on successful
  execution; Float products and divisions can underflow. `abs`, even
  powers, `sqrt` and `exp` are nonnegative; `abs` of a nonzero value is
  positive. `sqrt` of a positive finite value is positive. Infer stronger facts
  only when valid for the actual representation;
- `n()` is `> 0` within a nonempty group and `>= 0` over an arbitrary table;
- comparisons and presence tests in filters, `if` branches and match guards;
- validated contract assertions that entail the property;
- `positive(x)`, `nonnegative(x)` and `nonzero(x)` return the refined value
  or `None`; `ratio(a, b)` returns `None` for missing operands or zero divisor,
  otherwise `a / b`.

A missing domain proof is a pending obligation if it concerns a concrete,
pure computation over a fully known snapshot; otherwise it is a type error
unless a necessary dynamic check is explicitly supported for an effect-dependent
operation. During exploration, inspect all relevant present operands at the
operation's actual evaluation scope, respecting guards. Explicit locking accepts
successful snapshot-specific proofs; failed obligations identify the invalid
domain and suggest a filter or safe helper. A known-invalid constant is rejected
immediately. Unknown generic inputs retain domain constraints until instantiated.
Snapshot evidence cannot generalise into a function promise or establish a
refinement on unrelated rows, stages or inputs. The schema and diagnostics
identify static, contractual and snapshot-specific evidence separately.
Overflow and representability obligations follow the same proof lifecycle
(see [Dialects](dialects.md)); safe helpers do not turn overflow into missing data.

### Conversions

`T(x)` converts, wraps or unwraps explicitly. Conversion preserves value;
where it needs a representability check, failure is an error, not silent
rounding. Optional lifting preserves missing positions.

| From | To | Result |
| --- | --- | --- |
| `Int` | `Float` | exact value or precision-loss error |
| `Float` known to be whole | `Int` | same value, checked portable range |
| `Bool` | `Int`, `Float` | `FALSE` is 0, `TRUE` is 1 |
| any `showable` type | `String` | rupam display format |

The rupam display format writes `Int` in decimal, `Float` as the shortest
decimal that reads back to the same value, with `.0` on whole values and an
exponent from `1e16` up or below `1e-4` (`2.0`, `0.30000000000000004`,
`1e+300`), `Bool` as `TRUE` or `FALSE`, and `String` unchanged. REPL values
and interpolation use it in every dialect.
| newtype | its base type | unwrapped value |
| base type | newtype | wrapped value |

`floor`, `ceiling`, `round` with nonpositive digits, and `trunc` establish
whole Float results. `Int(round(x))` type-checks; `Int(x)` on an
arbitrary Float does not. Parsers explicitly return `T?` on invalid input
([Standard library](stdlib.md#parsing)).

## Records and rows

Records are immutable and structurally typed. A row type
`{revenue: Float, ..r}` contains `revenue` and any other fields. Table rows
are records. Field order is not part of a record or table type.

## Variants and enums

```text
type Quality = | Absent | Invalid | Zero | Valid
type Reading = | Measured(value: Float) | Offline | Fault(reason: String)
```

- A variant whose constructors have no fields is an enum. Enums are ordered
  by declaration. Their identity is nominal and stored values are distinct
  values of a single primitive type, normally constructor-name strings;
  explicit stored-value mappings follow Syntax.
- Variants may be stored in columns.
- Exhaustiveness is decided by Maranget's usefulness algorithm. `Bool`,
  `T?`, vectors by length and declared variants have finite heads.
  Numeric patterns are exhaustive when their ranges cover every value. Other
  types require a catch-all. A missing case is reported as a concrete
  pattern.

## Newtypes

```text
type PatientId = new Int
type Usd = new Float deriving (comparable)
```

- A newtype is distinct from its base type and from other newtypes, with the
  same representation.
- `PatientId(5)` constructs; `let PatientId(n) = id` deconstructs.
- A newtype is `equatable` if its base is. Ordering requires `deriving (comparable)` and a comparable base.
  Nominal values do not derive `number`; arithmetic and mathematical
  functions require explicit unwrapping. `deriving` supports only the
  declared built-in constraints, not arbitrary user-defined instances.
  Dimensional arithmetic is deferred.

## Dates and times

| Type | Meaning |
| --- | --- |
| `Date` | calendar date |
| `Time` | wall-clock time of day |
| `DateTime<Z>` | instant in time zone `Z` |
| `Duration` | exact length of time |
| `Period` | calendar amount in days, months and years |

- Date-times in different zones do not combine or compare. `to_zone`
  converts.
- Every date-time has a zone. A value read without one requires a zone from
  the contract, the inline schema or `assume_zone`.
- `Date` and `DateTime` do not compare.
- `DateTime + Duration` is exact. `+ Period` is calendar arithmetic; month
  arithmetic clamps to the last day of the month.
- Constructing a date-time from a wall-clock reading in a daylight-saving gap
  or overlap requires `nonexistent =` and `ambiguous =`.

## Effects

Functions are pure. Reading and writing files, the clock and randomness are
effects and appear only at top level. `set_seed(seed: Int)` sets the host
random stream (see [Dialects](dialects.md#effects-and-portability)). Top-level
effects evaluate once, in source order. Pure functions capture their values;
no effect is repeated by lifting or inlining.

## Bindings

Values are immutable. `let` binds a name; a later `let` with the same name
shadows it.
