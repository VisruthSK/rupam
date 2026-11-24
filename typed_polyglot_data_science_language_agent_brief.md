# Typed, Polyglot Data Science Language

**Agent implementation brief and proposed language specification**  
**Status:** design baseline, version 0.1  
**Working CLI:** `ds`; **working source extension:** `.ds` (both placeholders, not product names)  
**Implementation language:** OCaml  
**Initial execution/code-generation targets:** R with tidyverse, Python with Polars  
**Later targets:** Julia with Tidier.jl; optionally other engines  
**Contract format:** upstream `data-dict.yaml` specification, initially v0.1.0

## 0. Instructions to the implementation agent

Build a small, real programming language for interactive data analysis. It has an original, consistent syntax, an ML-family type system, inferred lifting over columns, a strong terminal REPL, strict input contracts, lazy table computations, and generated, editable R/Python scripts. It must feel coherent to someone who likes both OCaml and tidyverse, while carrying none of R's or Python's syntax and evaluation baggage into the source language.

This document distinguishes:

- **REQUIRED:** observable language behavior or delivery criteria; an implementation cannot depart from these silently.
- **DESIGN DEFAULT:** a selected implementation strategy that should be followed unless a concrete technical problem is recorded with a better alternative and regression tests.
- **FUTURE:** an interface or algebra to accommodate later, without implementing it in the first milestone.
- **UNRESOLVED RESEARCH:** a theoretical or engineering question that must not be presented as already solved. The brief gives a safe v0.1 behavior.

The agent must preserve semantic correctness ahead of native-code aesthetics or speed. Where an R or Polars operation differs from source semantics, emit guarded operations or small transparent compatibility helpers. If equivalent translation is not available, reject that target with a specific diagnostic. An explicitly permitted backend-specific operation is the exception; it is never silently made portable.

**Do not** implement a polished parser around fake typing, hard-code a few sample transformations as a substitute for an IR, or claim that successful type checking proves external data satisfy a contract before runtime validation. Produce a working vertical slice, then broaden it.

The language itself is the primary development environment. Code generation is a first-class deliverable, not an excuse to make the source environment awkward.

## 1. Product intent and constraints

### 1.1 The problem

Data analysts repeat conceptually identical transformations in R and Python, losing reusable abstractions and sometimes changing semantics along the way. Dynamic dataframe APIs make incorrect column names, null handling, invalid joins and schema drift easy to discover late. Existing ML languages provide strong inference but rarely offer an analyst-first exploratory environment.

The proposed language should let an analyst:

1. Open a data-dict contract and immediately obtain a typed view of its tables.
2. Explore and reshape tables in a terminal REPL, with completion, rich inspection, and immediate schema/type feedback.
3. Write ordinary composable ML-style functions that lift naturally over columns.
4. Execute any valid intermediate prefix while continuing to edit an incomplete pipeline.
5. Turn the exploration into a reproducible script in the source language.
6. Compile that script into readable native R/tidyverse or Python/Polars code, with warnings for expensive or backend-specific lowering.
7. Know that the generated code respects the language's defined semantics and runtime contract obligations.

### 1.2 Target users

Primary: experienced analysts who value R/tidyverse's fluid exploration but want static guarantees. Equally important: functional programmers who want an expressive statistical and tabular language. Supporting cross-language distribution is valuable, but this is not primarily a replacement for a cross-language ETL configuration file.

### 1.3 Fixed decisions

- New purpose-built grammar. OCaml inspires the *semantics*, not its surface punctuation.
- One ordinary expression and function-call syntax across domains.
- Hindley–Milner-inspired inference with row polymorphism, ADTs, pattern matching, constrained numeric operations, and explicit nullability.
- Automatic, predictable lifting over dataframe columns; no implicit `map` on ordinary linked lists.
- No reliance on positional dataframe indexing or implicit row order.
- Named columns, named ranges, and schema-aware selectors.
- Sequential derived-column bindings.
- Column names win inside table-expression scopes; explicit `^name` accesses outer lexical variables and `$name` explicitly accesses a column.
- Lazy execution where possible; eager interactive type/schema feedback and on-demand previews.
- Independently excellent terminal REPL. Editor and later notebook are clients of the same compiler/session infrastructure.
- Complete or partially complete programs remain useful in the editor. Invalid fragments never masquerade as successfully evaluated results.
- data-dict supplies external data contracts and metadata. Do not invent a parallel YAML format.
- Validate external data before using contract-dependent values as proven. Validation is a runtime obligation, not an excuse to block static exploration.
- R/tidyverse and Python/Polars are initial targets; Julia later. No Rust target.
- Modeling and graphics require architectural room but no first-version implementation.
- Backend escapes require explicit opt-in. A portable script that cannot be translated faithfully fails compilation to that target.
- A slower correct fallback is acceptable and should generate a meaningful performance warning.

### 1.4 Non-goals for v0.1

No general statistical model fitting, plotting grammar, SQL backend, distributed execution, multi-dimensional matrix indexing, `iloc`, GPU runtime, universal Python/R foreign-function interface, reversible source-code synchronization, arbitrary grammar macros, or full compiler optimization framework. Do not implement a notebook before the terminal REPL works. A full optimizing engine in OCaml would duplicate mature backend engines. Recursive definitions and general effects inside column expressions may be deferred; see the type and lowering sections.

### 1.5 Success measures

- An analyst can load a contracted table, inspect types, derive columns, group/summarize, and preview without writing a type annotation or manually constructing expression objects.
- Missing columns, incompatible types and unsupported null handling are rejected before execution, with source-span diagnostics and suggestions.
- The same checked transformation generates executable R and Python whose outputs satisfy shared semantic tests.
- The REPL can display the type, schema, contract status, plan, native translation and a rich paginated preview of any valid bound table or intermediate stage.
- A temporary syntax error or incomplete expression does not erase previously established definitions or prevent inspection of valid pipeline prefixes.

## 2. One coherent source-language experience

### 2.1 Canonical example

The following is **proposed normative surface syntax**. Exact library import and contract object names may be adjusted during bootstrapping, but the grammatical conventions and meaning must remain consistent.

```text
use contract "data-dict.yaml" as shop

let add_margin(revenue, cost) = revenue - coalesce(cost, 0.0)
let standardize(x) = (x - mean(x)) / sd(x)

let clean(orders) =
    orders
    |> filter(revenue > 0.0)
    |> derive(
         margin = add_margin(revenue, cost),
         z = standardize(margin),
         unusual = abs(z) > 2.0
       )
    |> select(customer_id, region, margin, z, unusual)

let orders = load(shop.orders)
let cleaned = clean(orders)

let regional =
    cleaned
    |> summarize(
         transactions = count(),
         total_margin = sum(margin),
         by = region
       )
    |> arrange(desc(total_margin), region)

save_parquet(regional, "regional.parquet")
```

Assume the contract declares `revenue: Float64` required, `cost: Float64?` optional, and `customer_id`/`region` required. `standardize` is a whole-column function; `add_margin` can be applied to scalar values or lifted over columns; `abs` is elementwise. `z` is nullable because `mean` and `sd` may be undefined on empty/insufficient observations. The comparison used for `unusual` therefore also yields a nullable Boolean. The final table retains that optional Boolean unless the analyst handles its missing values explicitly. The source must never silently convert missing values to zero.

These are ordinary functions. `derive` binds named expressions in sequence. `unusual` sees the `z` defined immediately before it. The compiler must lower this correctly even when Polars requires multiple `with_columns` stages.

### 2.2 Surface language principles

- Calls use `f(a, b, name = expression)` consistently. Commas separate arguments. No mixed tuple/list/record punctuation merely to spell ordinary table operations.
- A list is `[a, b]`; a record value is `{field = value, other = value}`; a tuple is `(a, b)`. These distinct data structures are available for their real purposes.
- `let f(x, y) = expr` introduces a function. `fn(x) => expr` creates an anonymous function. `|>` pipelines a value into an operation.
- `==` tests equality; `=` binds/assigns in declarations and named arguments. Expressions are immutable.
- Use `if condition then yes else no` and `match expression { pattern => value, ... }` for ML-style control flow.
- Names, types, modules and function definitions are first-class programming concepts, not a series of table macros.
- `derive`, `filter`, `select`, `summarize` and `join` are typed standard-library forms elaborated with column scopes, not five separate embedded languages.
- `derive` is the only name for add-derived-column behavior. Do not introduce `mutate` as a second synonymous operation. Column replacement is a distinct `replace` operation.

### 2.3 Column scope and captures

Inside table-expression arguments, a bare identifier in a *value position* refers to a column of the current schema if that name exists. `$name` always refers to a column. `^name` refers to an enclosing lexical variable. Names used as the callee of a function call resolve in the function/module namespace; a column and function can therefore have the same spelling without making `mean(revenue)` ambiguous. The LSP should report name collisions and show resolution on hover.

```text
let threshold = 100.0
let revenue = 999.0

let high = sales
  |> filter(revenue > ^threshold)       # revenue is column; threshold is captured
  |> derive(
       ratio = $revenue / ^revenue      # explicit column and outer variable
     )
```

For stability, the compiler must record the resolved symbol ID, not re-resolve names in later passes. If adding a column changes the binding of an existing bare name, issue a diagnostic suggesting explicit `$` or `^` qualification; a strict warning mode treats this as an error. An identifier absent from the column scope may resolve to an ordinary lexical constant only when that binding exists; otherwise it is an unknown-column/name error with close-name suggestions. Function definitions themselves use normal lexical scoping.

### 2.4 Sequential assignments

```text
sales |> derive(
  net = revenue - coalesce(cost, 0.0),
  rate = net / revenue
)
```

This is an ordered sequence of column bindings, not a simultaneously evaluated record literal. `derive` MUST reject redefining an existing column name; `replace` MUST require an existing name and replace it sequentially. The compiler's typed IR must represent each binding as a separate step (later optimization may fuse steps when safe).

### 2.5 Names, ranges and selectors

```text
select(region, revenue, cost)
select(region:cost)
select(starts_with("sales_"))
select(where(is_numeric))
select(-cost)
```

Named ranges mean fields between two named columns in the **schema's column order**, inclusive. Tables are unordered *by rows*, but schemas retain column ordering. Selectors are resolved statically when schema labels are known. If a selector's result cannot be known at compile time, the operation returns a dynamically shaped table requiring schema validation before statically named access, or fails in strict static mode. No integer column offsets or positional dataframe selection are exposed.

### 2.6 Row order is not implicit

`Table<Row>` has no stable row order. `filter`, `join`, `summarize`, `distinct`, and ordinary table values do not promise ordering. `arrange(desc(revenue), customer_id)` creates an explicit ordered view; tie-breaking is the programmer's responsibility. Operations that make order semantically relevant (`take`, `lag`, `lead`, running/rolling windows) must have an explicit ordering specification or consume a verified ordered view. A `preview` of an unordered table may show an arbitrary subset and must label it as such. An optimizer may reorder rows whenever doing so preserves the unordered-table semantics. Operations such as `join` and `summarize` discard any prior ordering guarantee.

## 3. Lexical and syntactic specification

The grammar below is the **v0.1 syntax baseline**. It is a source grammar, not a grammar for generated R or Python. Whitespace is insignificant outside tokens and strings; newlines terminate declarations only when a complete expression is present. The parser must support multiline pipelines and function calls. Comments begin with `#` and run to end-of-line; documentation comments may later use `##`.

### 3.1 Tokens and literals

- Identifiers: Unicode XID start/continue letters and `_`, with digits after the first character. Keywords are reserved. Backtick-quoted identifiers allow column names containing spaces or punctuation: `` `gross revenue` ``. Escape backticks inside quoted names according to the tokenizer spec.
- Numbers: decimal integer literals, floating literals with fractional or exponent parts, optional explicit suffixes where needed. Literal typing is constraint-based; a plain integer constant adapts to the expected exact integer type, but a general `Int64` value is **not** silently cast to a `Float64` value if precision could be lost.
- Strings: double quotes with escapes; dates and datetimes use explicit library constructors initially, avoiding ambiguous bare literals.
- `true`, `false`, `None`, `Some(expr)`; `None` needs contextual type inference.
- Operators: `+ - * / % == != < <= > >= and or not |>`, unary `-`, `^` for lexical capture in column scope, `$` for explicit column reference, `:` in types and named selectors.
- Table column name selectors and function labels are known at compile time. Dynamically constructed strings cannot silently become statically named columns.

### 3.2 Concrete EBNF baseline

This grammar fixes the declaration and expression forms the implementation must accept. Lexical details and token-to-newline joining are specified in 3.1 and 3.3. It is intentionally small: standard-library operations are function calls, and new statistical domains do not acquire new grammar productions. Constructor tags start with uppercase letters; a variant type uses a leading `|` to distinguish it from a type alias. The parser constructs a `selector_range` node for `a:b` in any expression; the type checker permits it only in selector-typed positions. Similarly, unary `-` can elaborate to selector exclusion only in selector-typed positions.

```ebnf
program          = { top_level } ;
top_level        = declaration | expr, statement_end ;
declaration      = contract_import | module_import | type_decl | let_decl ;
contract_import  = "use", "contract", string, "as", identifier, statement_end ;
module_import    = "import", dotted_ident, ["as", identifier], statement_end ;
type_decl        = "type", identifier, [type_params], "=", type_body, statement_end ;
type_params      = "<", identifier, {",", identifier}, ">" ;
type_body        = type_expr | variant_body ;
variant_body     = "|", constructor, {"|", constructor} ;
constructor     = upper_identifier,
                  ["(", [named_type, {",", named_type}], ")"] ;
named_type      = identifier, ":", type_expr ;
let_decl        = "let", identifier, [type_params], [parameters],
                  [":", type_expr], "=", expr, statement_end ;
parameters      = "(", [parameter, {",", parameter}], ")" ;
parameter       = identifier, [":", type_expr], ["=", expr] ;

expr             = lambda_expr | if_expr | match_expr | local_let | pipeline ;
lambda_expr      = "fn", parameters, "=>", expr ;
if_expr          = "if", expr, "then", expr, "else", expr ;
match_expr       = "match", expr, "{", branch, {",", branch}, [","], "}" ;
branch           = pattern, "=>", expr ;
local_let        = "let", identifier, [parameters], "=", expr, "in", expr ;
pipeline         = logical_or, {"|>", logical_or} ;
logical_or       = logical_and, {"or", logical_and} ;
logical_and      = comparison, {"and", comparison} ;
comparison       = selector_range, [("==" | "!=" | "<" | "<=" | ">" | ">="),
                   selector_range] ;
selector_range   = additive, [":", additive] ;
additive         = multiplicative, {("+" | "-"), multiplicative} ;
multiplicative   = unary, {("*" | "/" | "%"), unary} ;
unary            = ["-" | "not"], postfix ;
postfix          = primary, {"(", [arguments], ")" | ".", identifier} ;
arguments        = argument, {",", argument}, [","] ;
argument         = identifier, "=", expr | expr ;
primary          = identifier | "$", identifier | "^", identifier
                 | literal | "(", expr, ")" | tuple | list | record ;
tuple            = "(", expr, ",", expr, {",", expr}, [","], ")" ;
list             = "[", [expr, {",", expr}, [","]], "]" ;
record           = "{", [field, {",", field}, [","]], "}" ;
field            = identifier, "=", expr ;

pattern          = "_" | identifier | literal | record_pattern
                 | upper_identifier, ["(", pattern, {",", pattern}, ")"]
                 | "(", pattern, ",", pattern, {",", pattern}, ")" ;
record_pattern   = "{", identifier, {",", identifier}, "}" ;

type_expr        = type_postfix, ["->", type_expr] ;
type_postfix     = type_atom, ["?"] ;
type_atom        = dotted_ident, ["<", type_expr, {",", type_expr}, ">"]
                 | "{", [row_fields, [",", "..", identifier] | "..", identifier], "}"
                 | "(", type_expr, {",", type_expr}, ")" ;
row_fields       = named_type, {",", named_type} ;
dotted_ident     = identifier, {".", identifier} ;
literal          = integer | float | string | "true" | "false" | "None" ;
statement_end    = newline_at_top_level | ";" | end_of_file ;
```

`statement_end` occurs only after a syntactically complete top-level construct, not inside an open call, `match` or pipeline. A physical newline following `|>`, `=`, a binary operator, or a comma continues the current expression. The implementation must define and test tokenization for multiline `if` and `match`. `Some(x)` is an ordinary constructor call; `None` is a contextual literal. A backtick-quoted identifier occupies the same syntactic positions as an ordinary identifier. A pipeline `x |> f(a)` inserts `x` as the first positional argument, while `x |> f` means `f(x)`; it evaluates `x` exactly once. Other operators do not implicitly create table scopes.

The grammar does **not** define the meaning of `select`, `derive`, `plot`, `model`, or other libraries. Column-scope insertion, selector interpretation and typed aggregate contexts happen during elaboration, based on standard-library signatures. Syntax recovery may insert hole nodes beyond this complete-program grammar.

### 3.3 Operator precedence and evaluation

Highest to lowest: postfix calls/field access; unary `not`/negation; `* / %`; `+ -`; comparison; `and`; `or`; `|>`; then the outer `let`, `if`, `match`, and lambda forms. Infix arithmetic is left-associative. Comparisons are non-chaining in v0.1 (`a < b < c` is rejected; write `a < b and b < c`). Pipeline `x |> f(a, b)` means `f(x, a, b)` for the functions in this language's table-first API. It must not reorder named arguments or evaluate effects twice.

Function arguments and ordered derived bindings are evaluated in source order whenever evaluation has observable effects or may fail. A backend may vectorize or reorder pure, total expressions only if semantic equivalence holds, including defined failures. A potentially throwing branch of `if` must not be evaluated on an input for which that branch is not selected. This is important when targeting Polars conditional expressions, which evaluate both branches independently.

### 3.4 Formatting and recoverable syntax

Supply a canonical formatter as part of the language tooling. It must preserve comments, explicitly captured names, ordered bindings, and source mappings. Parser recovery creates error nodes/typed holes with source spans; it must not invent a valid runtime value for a missing expression. Syntax and type errors remain separate diagnostic categories.

## 4. Types, inference and static semantics

### 4.1 Core types

The initial user-visible type vocabulary is:

```text
Unit, Bool, Int64, Float64, String, Date, DateTime, Duration
Option<T>             (T? is optional display/annotation sugar)
List<T>               (ordinary immutable linked/list collection)
Record{a: A, b: B, ..row}
T1 -> T2              (functions; multi-parameter functions are curried internally)
Table<Row>             (unordered table with named typed columns)
Column<T>             (a whole-column computation/value in a context)
DynamicTable          (table whose full output schema is not statically known)
Result<T, E>          (errors returned as values)
```

First release may represent decimals, time zones, nested array shapes, and additional temporal/structured data internally and surface them in later milestones. Imported physical Parquet types that cannot be represented faithfully must produce a clear unsupported-type diagnostic; do not coerce them to strings or floats without consent.

Missingness is `Option<T>`; it is not encoded as a special floating value. `None` is separate from `NaN`, positive/negative infinity, zero, and empty strings. `Option<Column<T>>` and `Column<Option<T>>` are different types and never interchangeable implicitly.

The type system has immutable records, algebraic data types, pattern matching, type variables, generalized let bindings, and qualified numeric/equality/ordering constraints. Inference uses a documented, terminating extension of HM. Full principal types are an aspiration, not something to claim automatically after adding row, rank and type-class constraints; ambiguous constraints must produce diagnostics.

### 4.2 Row-polymorphic tables

A reusable transformation should infer only the fields it needs and preserve unrelated fields:

```text
let add_net(df) = df |> derive(net = revenue - cost)
```

Conceptual signature, with `net` absent from the input row:

```text
forall row.
  lacks(row, net) =>
  Table<{revenue: Float64, cost: Float64, ..row}>
   -> Table<{revenue: Float64, cost: Float64, net: Float64, ..row}>
```

The type checker must implement field-presence and field-absence constraints, row extension, selection, renaming, replacement, and joins without pretending a table is homogeneously typed. User annotations may include `..row`, but ordinary transformations should infer these types. Schema order is tracked separately from a table's row order.

For every operation, provide a documented schema transformer in the typed standard library. The IR should retain both input/output row types and proof obligations (e.g. key uniqueness or missingness checks). Row-type operations may be compiler primitives initially; do not hide their behavior behind unchecked callbacks.

### 4.3 Numeric operations and nullability

- Integer arithmetic has a fixed width, with checked overflow. R's ordinary double arithmetic is not an acceptable invisible substitute for exact 64-bit integer arithmetic.
- A numeric literal may be contextually specialized when it fits exactly; ordinary integer variables do not silently widen to floating types when that can lose precision.
- `/` is floating-point division after an explicit/safe numeric conversion. The language must define division-by-zero and overflow behavior, and tests must cover both; in v0.1 use IEEE-754 division for `Float64` and an explicit checked conversion for integers.
- `Float64` arithmetic follows specified IEEE-style NaN/infinity rules, subject to a documented cross-backend tolerance for non-associative reductions. Never claim bitwise-identical summation when engines use different parallel reduction orders.
- `f : A -> B` applied to `Column<Option<A>>` is lifted strictly to `Column<Option<B>>`: null inputs produce null outputs without invoking `f` on the missing element.
- `g : Option<A> -> B` receives missingness explicitly and may produce a nonoptional result.
- A plain `A` may be injected as `Some(A)` when an expected `Option<A>` argument makes that conversion unambiguous. There is no implicit conversion from `Option<A>` to `A`.
- `coalesce`, `is_some`, `is_none`, and `match` allow explicit handling. The compiler must never auto-fill missing numeric values with zero.

For Boolean filtering, **v0.1 chooses strictness:** `filter` requires a nonnullable Boolean predicate. `filter(is_some(value) and coalesce(value > 0, false))` is valid; `filter(value > 0)` is rejected if `value` is nullable and produces `Option<Bool>`. A future `filter(..., missing = drop)` convenience is possible but may not silently change this default.

### 4.4 Expression shapes and context

Value types alone cannot describe data analysis. Internally distinguish:

- scalar/constant expressions;
- row expressions aligned to a particular table expression;
- whole-column expressions;
- aggregates over the entire input or over named groups;
- window expressions with explicitly declared partitions and order.

These are typed computational contexts, not user-visible punctuation or separate mini-languages. A table expression's column can be represented in IR as `ColExpr<Row, T, ContextId>`. Context IDs prevent accidental alignment of columns from unrelated tables merely because their lengths match. Combining columns from independent tables requires `join` or another explicit relational operation.

A whole-column function can be inferred naturally:

```text
let standardize(x) = (x - mean(x)) / sd(x)
```

Here `mean` and `sd` require a column, so `x` is inferred as a columnar input. `mean` and `sd` are reductions with scalar, potentially missing results. Their results broadcast to the row context in subtraction/division. In `derive(..., by = region)`, they are evaluated within each region's group. Outside grouping, they cover the whole table.

A scalar function remains a scalar function:

```text
let squared(x) = x * x
squared(3)
squared(sales.revenue)
```

The second call is elaborated into an elementwise computation, not a scalar call that mistakenly receives a Polars expression object.

### 4.5 Inferred lifting: v0.1 algorithm

Automatic lifting is required, but hidden heuristics are not. For any function application:

1. Infer argument types and shapes, including the table provenance/context of each column.
2. If the function's declared/inferred signature accepts those shapes directly, apply directly (e.g. `mean(column)`).
3. Otherwise, attempt a unique **elementwise lifting** of a scalar, pure function over one or more same-context columns. Scalars broadcast to each row. A function with explicit `Option` arguments is invoked with option values; one with ordinary scalar arguments uses strict null propagation.
4. If expected types make an optional injection unambiguous, insert it during elaboration. Record each inserted lifting/injection node in the typed IR.
5. If multiple elaborations are equally valid or column contexts conflict, reject as ambiguous. Show the alternatives and a remedy; never pick an implementation based on whether R or Polars happens to accept an input.
6. Do not implicitly map over `List<T>`; `map(f, values)` remains explicit.
7. For v0.1, support rank 0 scalars and one-dimensional table columns. Keep a typed extension point for Julia-style rank-polymorphic broadcasting over arrays, but do not claim that general multidimensional shape inference works before it is implemented.

Examples that **must** type-check with appropriate concrete schemas:

```text
let f(x, y) = x + y
f(1, 2)
f(revenue, 2)
f(revenue, discount)
let normalize(x) = (x - mean(x)) / sd(x)
normalize(revenue)
```

The third case has aligned column inputs; if `discount` is nullable, the output is nullable. The final example is a whole-column operation, not elementwise `normalize` on each scalar row. Document which standard-library functions are reducers versus elementwise functions. A backend can fuse elementwise operations or lower a pure user-defined function through a callback without changing the source-level rule.

### 4.6 Aggregates, nulls and empty inputs

For v0.1, choose the following source-language semantics, implemented consistently by all targets:

- Aggregates skip missing input observations, except `count()` which counts rows and `count_present(x)` which counts nonmissing values.
- `sum`, `mean`, `min`, `max` return `Option<T>` (or `Option<Float64>` for `mean`) and return `None` when no nonmissing values are available. `sd` returns `None` when fewer than two nonmissing values exist; it uses the sample standard deviation (`n - 1`) consistently.
- `count()` always returns a nonnullable integer, including zero for an empty input.
- Missing values in grouping keys form one group. NaNs are not missing; grouping-key equivalence treats NaNs consistently as one equivalence class. Ordinary floating equality retains IEEE behavior (`NaN == NaN` is false). This requires separate comparison and key-equivalence semantics.
- Arithmetic with a missing result propagates missingness. Empty/all-null aggregates do not silently use R/Polars native identities when those disagree with the source specification.
- Three-valued logic may be modeled through `Option<Bool>`, but control flow and filtering must make how unknown values are handled explicit.
- `if` is branch-selective at the source level, including inside lifted column computations. Backends must use masking/guarding or a pure callback when their native conditionals evaluate unsafe nonselected branches.

A reference evaluator on small in-memory tables defines the expected answers. Guarded code-generation paths and compatibility helpers exist to reproduce these answers. Changes in numerical policy require a language-version change and new conformance fixtures.

### 4.7 Keys, relationships and cardinality

Track key constraints as table refinements with provenance: declared, metadata-checked, value-validated, inferred by safe transformation, or unknown. A validated `primary_key` is nonnullable and unique; a validated `unique` field is unique among present values but may still be nullable according to data-dict's rules. Composite keys are essential.

`filter` preserves previously established uniqueness constraints. `derive` preserves keys unless it replaces relevant columns. `summarize(..., by = (a, b))` establishes uniqueness of its grouping key tuple, subject to the null/grouping policy. A join can multiply rows depending on cardinality; preserve uniqueness only when the join's verified cardinality proves it. This is an analysis aid, not permission to assume unvalidated external constraints.

### 4.8 Dynamic schema boundary

Some reshaping operations, especially a wide pivot whose field names depend on runtime values, cannot have a fully known output row type. Such an operation must either (a) require an explicit finite set of output labels or (b) produce `DynamicTable`. A `DynamicTable` can be inspected and validated against a declared schema; it cannot be accessed using statically named columns until validation establishes the schema. No guessed types from one preview sample become permanent proofs.

### 4.9 Effects and recursive functions

Functions are pure by default. Loading/saving data, reading the clock, randomness without an explicit generator, and target-specific calls are effects tracked separately from the pure expression IR. Backend callbacks for column operations must be pure and deterministic with respect to their input; Polars may call them on arbitrary batches. Ordinary top-level scripts may sequence I/O operations.

General recursion may be deferred in v0.1. If supported later, recursion does not automatically become vectorizable; a pure recursive function may compile to ordinary native code with a performance warning. Neither recursion nor an effectful operation may be silently executed per row using unsafe side effects.

## 5. data-dict: contracts, metadata and trust boundaries

This is a critical integration, not a home-grown approximation of data-dict.

### 5.1 Keep the upstream format intact

The current data-dict specification is pre-1.0 (version 0.1.0 at the time of writing). It defines dataset/table/column metadata, Parquet sources, constraints, relationships and reusable definitions. **Do not add private YAML keys to `data-dict.yaml` to accommodate the compiler.** Consume the upstream spec as written and preserve unknown future-compatible metadata when exporting or manipulating it. Pin an explicitly tested version and report version incompatibilities.

Prefer the upstream CLI's structured interfaces over reimplementing its parser:

- `data-dict validate-spec` for specification validity;
- `data-dict validate-meta` for physical column/type compatibility;
- `data-dict validate-data` for values, uniqueness, referential integrity and assertions;
- `data-dict export-spec` for a fully resolved JSON view of table/column/relationship/definition metadata;
- JSON validation reports, including upstream `S##`, `M##`, and `D##` codes, for machine-readable failures.

The upstream CLI's `export-data` adds profiling but **is not a substitute for value validation**. Treat any `unevaluated` validation step, missing source, unsupported assertion or warning that means a claimed property could not be checked as *not proved*. Do not promote a key/requiredness/refinement to a verified fact on that basis.

### 5.2 Meaning of declarations

- `required`: value-level non-nullness, proven only by successful validation (plus metadata checks as relevant).
- `unique`: nonnull values are distinct; multiple nulls are permitted in data-dict. Do not infer `required` from `unique`.
- `primary_key`: the single or composite key is required and uniquely identifies rows.
- `foreign_key`: referential integrity according to the declared relationship.
- Enumeration `values`: allowed-value restriction.
- `assert`: enforceable expression, subject to upstream validator support and dialect fidelity.
- `range`, examples, descriptions, labels and units: metadata, not extra enforceable numeric bounds. In particular, a data-dict representative `range` does not become a runtime assertion.
- `display: restricted`: hide from default previews, completion value samples, diagnostics containing data, exports/downloads and logs. It is a presentation/privacy directive, **not an access-control or security mechanism**; explicit restricted-data viewing requires an intentional local override and stays out of telemetry.
- `number(id)`, `number(ordinal)`, `number(quantity)` describe analytical measure/role; they do not fix `Int64` versus `Float64`. Use Parquet physical metadata for that distinction.
- A declared column with no type exists in the dictionary but provides no checked value type; it must not be used as a fully statically typed field.

The compiler should attach descriptions, units, roles, enum values and relationship information to its symbols. Use these in hover text, inspection, completions and generated documentation. Never imply that metadata labels are compile-time proofs.

### 5.3 Schema importing and physical types

`use contract "data-dict.yaml" as shop` creates typed, contract-backed references such as `shop.orders`. The compiler obtains the dictionary schema through upstream resolved JSON, checks Parquet metadata, refines number/string/temporal representations into concrete types, and records the data contract provenance. `load(shop.orders)` returns a *lazy, contract-guarded table* whose row schema can be checked statically. No data values are claimed verified yet.

Schema reflection is itself typed. A column without a validated non-null guarantee remains `Option<T>` even if one sample happens to contain no missing values. A source whose declared/physical type cannot be mapped faithfully into the core type system is rejected for typed access with a precise diagnostic. Later language versions may add `Decimal`, nested `List` and `Struct`, categorical and timezone-aware primitives.

### 5.4 Validation under lazy execution

Separate three statuses and expose them distinctly:

```text
Contract: valid / invalid
Metadata: checked / mismatched / unavailable
Values: pending / verified / failed / incomplete
```

Rules:

1. Contract import validates the specification. A local contracted load checks source metadata as soon as feasible without scanning full data.
2. Static type checking may proceed *against the declared contract*, with the proof obligation attached to the lazy source node. It must not label that source "value-verified" before scanning it.
3. Before exposing materialized data, saving results or running an effect that relies on the contract, validate **the entire contracted source snapshot** and any related tables necessary for referential integrity. Validate before filter/projection pushdown can hide bad source rows or columns.
4. If validation fails or is materially incomplete, abort execution. The REPL may still show types, plans and last valid results from another snapshot. It may offer an explicitly marked **untrusted preview mode** for exploration, but such a preview cannot satisfy proofs or feed strict compilation/output.
5. Cache successful validation only for the same immutable data snapshot, matching contract version/hash and source identities. Changing a file, glob expansion, contract or backend execution assumptions invalidates the cached proof. Guard against time-of-check/time-of-use races by using immutable snapshots or a documented equivalent mechanism.
6. Revalidation must be possible explicitly (`:validate`) and invoked automatically at execution boundaries in strict mode. It is not mandatory to scan the entire dataset every time a user types a character.
7. Generated R/Python scripts must preserve the validation preflight and failure semantics; it is not enough for the source-language REPL alone to validate. They may call a pinned `data-dict` CLI or package a semantically equivalent validator, with that dependency documented in the generated manifest.

A result is statically well-typed conditional on the source contract. Validation discharges the input proof obligation. This language does **not** promise to prove arbitrary runtime properties of unknown files at compile time.

### 5.5 Reuse upstream definitions and relationships

data-dict `definitions` can describe filters, derived values and metrics, distinguished by type and row/aggregate shape. Import definitions when their expression and fidelity can be translated faithfully into this language's typed expression IR. Keep their names, documentation and provenance. Do not evaluate definitions during validation; upstream data-dict checks their well-formedness but does not treat their values as constraints.

Some data-dict assertions and definitions can be authored in R/Python syntax, and data-dict grades translations as exact, guarded, divergent or unsupported. A divergent/unsupported translation is not a portable proof. Require an explicit, target-specific escape for such a definition, or refuse import into the portable core. Portable definitions must use exact/guarded semantics.

Import relationship names, join predicates, cardinality and conflict metadata as typed hints and, where independently validated, constraints. No relationship's cardinality becomes an established proof solely because YAML declares it.

### 5.6 First-fixture data dictionary

Use an actual upstream-valid `data-dict.yaml`, rather than a bespoke contract, for the tutorial and tests. A representative fragment (verify against the pinned upstream schema before adopting) is:

```yaml
$version: "0.1.0"
$learn_more: https://data-dict.tidyverse.org/
name: shop

tables:
  - name: orders
    source:
      parquet: data/orders.parquet
    columns:
      - name: customer_id
        type: number(id)
        constraints: [required]
        examples: [101, 102]
      - name: region
        type: string
        constraints: [required]
        examples: [north, south]
      - name: revenue
        type: number(quantity)
        constraints: [required]
        range: [0, 5000]
      - name: cost
        type: number(quantity)
        range: [0, 5000]
```

Here `range` documents representative values; it does not enforce positivity. Add an explicit `assert` for that contract. Test both the upstream CLI's interpretation and our imported metadata.

## 6. Standard table and analysis library

### 6.1 Core table operators

The table API is intentionally small. Each operation has a typed input/output schema transformation, semantics independent of backend, source-span annotations and a reference implementation for small tables.

| Operation | Normative meaning |
|---|---|
| `filter(predicate)` | Retain rows whose **nonnullable** Boolean predicate is true; no ordering promise. |
| `select(selector, ...)` | Retain/reorder/rename named fields, resolved against schema. |
| `derive(name = expr, ...)` | Add new columns in **source order**; new fields may reference earlier fields; reject collisions. |
| `replace(name = expr, ...)` | Explicit sequential replacement of existing named fields; reject absent fields. |
| `summarize(name = aggregate, ..., by = selector_or_tuple)` | Return one row per group, with only grouping keys and aggregations. Ungrouped summary returns a single row even for an empty input. |
| `arrange(key, desc(key), ...)` | Construct an explicitly ordered view; ties unspecified unless keys resolve them. |
| `join(other, on = ..., how = ...)` | Combine tables on explicit keys/relationships, checking key types, collision policy, nullability and cardinality. |
| `distinct(column_selector...)` | Retain one row per distinct key; representative choice is nondeterministic for unselected columns unless specified explicitly. |
| `pivot_longer` | Reshape named fields into key/value pairs with checked resulting value types. |
| `pivot_wider` | Statically typed only with enumerated output fields; otherwise dynamic schema boundary. |
| `rename(old = new)` | Rename existing columns without changing their values. |

`group_by` may exist as a higher-order helper later, but persistent grouped-table state is **not** part of v0.1. `summarize(..., by = region)` and `derive(..., by = region)` explicitly define aggregate partitioning. This avoids inheriting incompatible backend grouping lifetimes.

A `derive(..., by = region)` expression may combine a row value and an aggregate over its group, such as `relative = revenue / mean(revenue)`. The compiler must lower that as a partitioned/window-like expression or a semantics-preserving join back to the original rows.

### 6.2 Joins

Supported v0.1 join kinds: `inner`, `left`, `right`, `full`, and (when feasible) `semi`, `anti`. Example:

```text
joined = orders |> join(customers, on = customer_id, how = left)
```

For differently named keys, permit a typed predicate in a special join scope, e.g. `on = left.customer_id == right.id`. Reject an implicit join over all common field names; matching names alone are not enough to establish meaningful relationships. A relationship defined in data-dict may be referred to by name.

- An unmatched side of an outer join becomes nullable.
- Null join keys do not match by default (unless an explicit `nulls_equal = true` is supported and requested). This differs from some backend defaults, so lowering must guard it.
- Collision of nonkey column labels is a compile-time error unless explicit renames/suffixes resolve it.
- Cardinality constraints are checked/validated where possible and preserved only through justified transformations; no accidental many-to-many join is assumed safe.
- Row order after join is unspecified.

### 6.3 Column selectors and reshaping

Name-oriented operations are mandatory. Implement `select(x, y)`, `select(x:y)`, exclusion `select(-x)`, `starts_with`, `ends_with` and type-driven selectors such as `where(is_numeric)` for known schemas. Selections are resolved at type-check time, unless the operation intentionally yields `DynamicTable`. An unknown field produces a compiler error rather than a late backend exception. For v0.1, support a deliberately documented subset of reshaping before claiming parity with tidyr.

### 6.4 Analysis abstractions beyond manipulation

Reserve but do not fully implement:

- `Metric<Row, T>`: reusable aggregation specified independently of the group it runs over;
- `Dimension<Row, K>`: named/grouping dimension or key extractor;
- `Comparison<...>`: contrasts, deltas and ratio-of-metrics operations;
- `Plot<Row>` and typed graphical transformations;
- `ModelSpec<Input, Output>` / `FittedModel<Input, Output>`.

These are future **libraries/algebras** on top of the same expression grammar, not new parsers. Do not pretend v0.1 offers full grammars of modeling or graphics. Design the IR plugin boundary so future domains can have their own typed nodes, capability checks, preview handlers and backend lowerers without touching the lexical grammar.

## 7. Backend-neutral semantics and generated code

### 7.1 Typed IR architecture

Compile through a series of explicit, typed representations:

```text
source text
  -> lossless, recovery-capable syntax tree
  -> name-resolved tree
  -> constrained type/shape inference
  -> typed functional core IR
  -> typed relational IR (with schema and provenance)
  -> backend-specific lowering
  -> readable native R or Python script + source map + manifest
```

A reference evaluator interprets the typed core and relational IR for small fixtures. The REPL may also execute compiled fragments in a persistent backend session for large data. Do not infer target semantics by executing a host-language function at compile time.

Each relational IR node should carry: operation kind, typed input schemas, output schema, nullability, keys/relationship refinements, source spans, context/group identity, ordering (if explicitly established), contract-validation obligations, purity/totality properties, and target capability metadata. These annotations permit later optimizations without redefining meaning.

### 7.2 Correctness and fidelity policy

Use the following target assessment categories:

- **exact:** native lowering has the required behavior within specified numerical policy;
- **guarded:** emitted checks/extra code ensure the required behavior;
- **fallback:** generated pure callback or less-efficient operation preserves behavior, with performance warning;
- **unsupported:** cannot preserve behavior using permitted generated code, so compilation fails;
- **backend-specific:** allowed only with an explicit user opt-in and a recorded nonportable dependency.

Never generate code that silently changes missingness, grouping, NaN behavior, integer overflow, join matching or the meaning of a conditional. Do not equate "the target library has a similar method name" with semantic equivalence.

### 7.3 R/tidyverse target

Generate ordinary, formatter-friendly R with pipes, `dplyr` verbs and clear intermediate functions. Use native vector operations when they preserve source semantics. Pure user-defined functions that are not automatically vectorized in R must be lowered through explicit elementwise mapping or a safe column-wide implementation. Do not compile a scalar `if` into an R `if` over a vector. Use guarded vectorized expressions or map appropriately.

Native R does not offer the same exact 64-bit integer arithmetic as the source language by default. Use a documented compatible representation/helper or reject an operation that cannot be made exact. Generated scripts are permitted a small, transparent helper module and a pinned data-dict validation preflight; they must not depend on running the compiler itself.

### 7.4 Python/Polars target

Generate clean `polars` LazyFrame pipelines and native `Expr` operations where possible. Polars UDF/callback APIs are a **fallback**, not the default lowering of all functions. They require declared return types and purity, and may be invoked on batches. Whole-column functions (e.g. `standardize`) must not be naively sent to a batch callback if batch boundaries would change their meaning; use an aggregate/window expression or materialize a whole column at an explicit boundary.

Polars' conditional expressions may evaluate both branches: unsafe branches must be guarded or lowered through a semantics-preserving fallback. Sequential `derive` bindings may require successive `with_columns` calls. Report materialization boundaries and callback warnings in `:explain` and build diagnostics.

### 7.5 Generated script contract

The default output is a standalone **analysis script**, not a library package. Generated output must:

- contain readable definitions, target-library calls, comments only where useful and a visible entry point;
- use deterministic names and formatting for stable source diffs;
- include/source a small generated compatibility helper only when necessary;
- enforce input contract validation before executing contract-dependent computations;
- provide a source map from generated expressions to original `.ds` spans;
- emit a machine-readable manifest: compiler/language version, target, backend dependencies, data-dict version, fidelity classes, performance warnings, required validation steps, and any opt-in nonportable calls;
- be executable without the source language compiler, given the documented target and validation dependencies;
- return comparable outputs for shared conformance fixtures.

A generated script may be manually edited, but such edits are outside the source language's guarantees. No reverse synchronization is promised.

### 7.6 Target-specific escape hatch

Allow explicitly marked backend operations, but never implicit fallback to arbitrary R/Python expressions. Define a typed foreign-operation declaration mechanism that names the target, imported symbol, argument/return schema, purity and required dependencies; enforce opt-in at source or CLI level. A module containing such a call is nonportable to unsupported targets, with a diagnostic naming the exact dependency. v0.1 may support only a small registry of these declarations; arbitrary inline Python/R evaluation is not required and must not be advertised as safely typed.

### 7.7 Optimization later

Prepare typed relational IR for projection pruning, predicate pushdown, safe column fusion, common-subexpression elimination, aggregate/window lowering and join planning. Initially let R/Polars handle native optimization. Any source optimizer must preserve row-unordered semantics, nullable/NaN behavior, exception paths, value-validation obligations, and order requirements. A rewrite that moves `filter` before contract validation is invalid if it can mask a bad source row.

## 8. Interactive environment: the primary product

The terminal REPL is a first-class application. It must be usable without VS Code, a notebook, a browser-only workflow or an RStudio extension. A language server and reactive notebook can use the same underlying services, but neither substitutes for an excellent REPL.

### 8.1 Kernel responsibilities

The session owns: source fragments and revisions; resolved bindings/types; typed IR; dependency/provenance graph; contract/snapshot statuses; backend process handles; cached results; and rendering preferences. Keep the compiler and kernel APIs separate so scripts, REPL and future notebooks compile identically.

An accepted definition has a versioned binding. Editing/redefining it does not mutate previously computed results silently; affected values become invalidated/stale and are reevaluated on explicit demand or according to the interface's documented reactive policy. Every displayed preview belongs to a particular source revision and data snapshot. When incomplete source is present, retain the last valid binding and show its status rather than replacing it with an invalid result.

### 8.2 Required commands and capabilities

The exact command prefixes may be bikeshedded once, but choose stable names and implement them consistently:

```text
:type expression        Inferred type and constraints without executing data
:schema expression      Named fields, physical types, nullability, keys, units
:inspect expression     Paginated rich table viewer
:preview expression     Bounded, explicit data execution and display
:contract expression    Contract source, versions, validation provenance/status
:validate expression    Execute outstanding data-dict obligations
:explain expression     Typed relational plan, lowering and performance warnings
:code r expression      Show generated R
:code python expression Show generated Python
:backend r|python       Select execution backend, recompile checked definitions
:reload                 Reload changed source/contract
:help                   Contextual help and discoverable commands
```

Commands that can run large computations must show the intended operation/estimated cost when available and offer cancellation. `:type` and `:schema` must never force a dataset scan. `:inspect` and `:preview` do materialize bounded data and therefore enforce the relevant contract first, unless explicitly placed in labeled untrusted exploration mode.

### 8.3 Rich dataframe inspection

The standalone inspector must provide horizontal/vertical navigation, pagination, schema/type headings, nullable-column indication, key/relationship information, units/descriptions, missing counts when known, safe sorting/filtering controls and quick copy/export of allowed fields. Default previews must exclude `display: restricted` values, including diagnostics and sample tooltips. The renderer distinguishes `None`, `NaN`, infinity and empty string. Preview row order is not treated as stable unless the query established an order.

An interactive stage inspector should let the user select any valid pipeline prefix and view its **inferred output schema** immediately. Showing values for that prefix triggers evaluation only of what is necessary, including validation. The inspector should identify materialization barriers and reuse valid cached results for an unchanged source snapshot.

### 8.4 Incomplete programs and typed holes

Partial code is an expected editing state, not a catastrophic compiler error. Required behavior:

```text
orders
  |> filter(revenue > 0)
  |> derive(
       margin = revenue - cost,
       z = standardize(
```

The parser should recover an AST containing a hole for the missing argument/expression. The checker retains the schema of the complete `filter` stage and, when possible, the type of the completed `margin` binding. It may display constraints on the `z` hole. It must not execute the unfinished `derive` or pretend the hole has a value. The user may preview the complete filter prefix and inspect any valid bound data from earlier revisions.

The initial implementation does **not** need the full theoretical semantics of Hazel's maximally live incomplete programs. A principled conservative recovery model is sufficient if it preserves useful completions and valid-prefix execution, never fabricates data, and labels uncertainty accurately. Improve to richer live typed-hole evaluation later.

### 8.5 Types and diagnostics as interaction

- Type-on-hover and `:type` include polymorphic constraints, column shape, and any inserted lifting. Types should be concise by default and expandable to the full scheme.
- Completion is schema-aware and uses data-dict descriptions, units and enumerations; it never leaks restricted sample values.
- Errors explain the specific schema mismatch: missing field, wrong type, unexpected nullability, incompatible table provenance, unsupported lowering, or unverified contract.
- For nullable arithmetic, point at the nullable input and suggest `coalesce`, `match`, or an optional-aware function. Do not report only raw unification variables.
- Every diagnostic includes original source span and stable machine-readable code. Generated-backend exceptions should map to source spans when possible.
- The REPL should expose query plans, callback/materialization warnings, and generated code rather than relying on opaque hidden execution.

### 8.6 Reactive notebook (FUTURE)

The future notebook uses dependency analysis on typed bindings, inspired by marimo, with automatic or stale-on-change recomputation policies. It shares a kernel with the REPL but does not change the REPL into a spreadsheet-like execution model. Notebook cell order must not redefine program semantics. Cache invalidation must account for source versions, data contracts, snapshots, backend settings and effectful operations. Store notebooks in a text-friendly format with an export to ordinary `.ds` source.

## 9. CLI, project and developer workflow

Choose `ds` and `.ds` as temporary implementation identifiers. Do not invent a branded name in the compiler design.

```text
ds check analysis.ds
ds fmt analysis.ds
ds repl --backend r
ds run analysis.ds --backend r
ds build analysis.ds --target r --out build/analysis.R
ds build analysis.ds --target python --out build/analysis.py
ds explain analysis.ds --target python
ds contract validate data-dict.yaml
```

`check` parses, resolves, type-checks and prepares contracts without reading full data (except metadata as permitted). `run` executes the checked script through the selected backend and enforces validation. `build` emits code, source map, manifest, and any compatibility helpers, without silently executing data. `explain` reports the typed plan, fidelity and potential fallbacks. `fmt` is deterministic.

Use a minimal project manifest to pin the language/compiler, backend versions and source files. This project manifest is **not** a replacement for the data-dict YAML contract. Ordinary scripts remain runnable without a project manifest in simple local scenarios.

Source extension, CLI name and exact manifest filename may change once in an early documented ADR. Surface syntax and semantics should not drift based on tool naming preferences.

## 10. Compiler and repository architecture

### 10.1 Suggested OCaml modules

```text
frontend/
  lexer               tokenization and source spans
  parser              recovery-capable grammar and typed-hole nodes
  ast                 surface AST and resolved names
  formatter           canonical formatting

types/
  core_types          primitive, function, ADT, option and record types
  row_solver          row presence/absence and polymorphism
  constraints         numeric/ordering/equality constraint solving
  shape_inference     scalar/column/aggregate/window contexts, lifting
  diagnostics         precise errors and recovery types

contracts/
  data_dict_client    upstream CLI, export-spec and validation JSON
  source_registry     physical schema and source locations
  validation_cache    snapshot proofs and invalidation

ir/
  functional_ir       typed scalar/functions/ADTs
  relational_ir       typed named-schema table algebra
  reference_eval      small correct evaluator
  lowering            capability/fidelity/fallback selection

backends/
  r_codegen           tidyverse and helper generation
  polars_codegen      Python/Polars and helper generation
  common              source maps, manifests, generated-code diagnostics

kernel/
  session             revisioned bindings, dependency graph and cache
  worker_protocol     R/Python execution processes, cancellation
  repl                terminal front end and inspector
  lsp                 incremental language-service client (later)

cli/
  check run build explain fmt contract repl

tests/
  parser typing semantics contract frontend diagnostics
  r_conformance python_conformance repl_integration
```

Use `dune` and established OCaml parser infrastructure (Menhir is a reasonable default). Prefer an incremental parsing API and explicit source spans from the start. Recovery, types and code-generation correctness are more important than committing to a particular editor UI toolkit in advance.

### 10.2 Backend worker protocol

The kernel sends checked definitions/native fragments to a persistent R or Python process, manages typed object handles and receives bounded previews, statuses and errors. Do not convert entire large dataframes to JSON for every preview; serialize only bounded visible pages and metadata. Arrow may provide an efficient interchange format when necessary, but no cross-language Arrow bridge is a v0.1 prerequisite.

Every request includes a session/revision ID, source-map identity, cancellation token, materialization policy, contract snapshot proof and capability requirements. A stale response is never displayed as belonging to the newest code revision. A backend error should identify the translated expression and link back to the source location.

### 10.3 Testing philosophy

The reference evaluator is the behavioral authority for small pure programs and tabular fixtures. Backends must agree with its results under the explicitly documented numeric comparison policy. Imported data-dict constraints must agree with the **pinned upstream validator**, rather than duplicating its rules approximately.

Implement differential tests, property-based table generation, grammar round-trip tests, type-checker soundness tests where feasible, regression fixtures and golden generated-code snapshots. Treat warnings and failure diagnostics as testable outputs, not optional polish.

## 11. Required conformance and acceptance cases

The agent must turn the following into executable fixtures. Expected behavior is part of the specification; the sample syntax uses the proposed grammar.

| ID | Source or scenario | Required outcome |
|---|---|---|
| T01 | `let square(x) = x*x; square(3)` | Scalar `Int64` result `9`, subject to checked arithmetic. |
| T02 | `derive(s = square(revenue))` | Automatically lifted numeric column; type inferred. |
| T03 | `derive(s = square(nullable_cost))` | `s` is nullable; missing values remain missing. |
| T04 | `derive(s = coalesce(nullable_cost, 0.0))` | `s` is nonnullable; explicit zero replacement. |
| T05 | `derive(a = revenue + 1, b = a * 2)` | Sequential binding; `b` uses the new `a`; native backends agree. |
| T06 | `derive(revenue = revenue + 1)` | Error: existing field; suggest `replace`. |
| T07 | `filter(nullable_cost > 0)` | Static error: nullable predicate; do not silently drop missing. |
| T08 | `select(region:cost)` | Select named inclusive schema range; no row-order implication. |
| T09 | `select(missing_column)` | Static unknown-column diagnostic with suggestions. |
| T10 | `let revenue = 3; derive(a = revenue, b = ^revenue, c = $revenue)` | `a,c` refer to column; `b` to lexical value, with resolved-symbol diagnostics. |
| T11 | `let normalize(x) = (x - mean(x))/sd(x)` applied to a column | Whole-column/aggregate semantics; result typed nullable where appropriate. |
| T12 | `f(col_a, col_b)` from different unrelated table contexts | Static provenance/alignment error unless explicitly joined. |
| T13 | `sum(x)` on empty/all-null group | Returns `None`; R and Polars must not substitute `0`. |
| T14 | `summarize(n = count())` on an empty ungrouped table | One row with nonnullable `n = 0`; `summarize(..., by = region)` on empty input yields zero groups/rows. |
| T15 | Join with nullable key and missing rows | Null keys do not match by default; outer-side fields become nullable. |
| T16 | Join with duplicate nonkey field labels | Static collision error until renamed or explicitly resolved. |
| T17 | `distinct(x)` with several `None` and `NaN` values | Key-equivalence semantics distinguish missingness from NaN and treat each consistently. |
| T18 | `arrange(desc(value), id) |> take(5)` | Explicit ordered result; no implicit ordering elsewhere. |
| T19 | `take(5)` on an unordered table | Static error or explicit warning/failure requiring order; not a deterministic top five. |
| T20 | Contracted source with a required-column null | Full data validation fails before output, even if `filter` would remove the row. |
| T21 | Contract with `range: [0, 100]`, observed value 101 | No failure solely for exceeding representative range; an explicit assertion is needed. |
| T22 | Source changed after successful validation | Validation proof invalidated; no stale verification badge. |
| T23 | Restricted column in a contracted source | Hidden from default preview/value completion/export and errors containing samples. |
| T24 | A partial `derive(..., z = fn(` expression | Parser recovery; valid prefix and earlier bindings remain inspectable; incomplete stage not executed. |
| T25 | Pure function unsupported as native Polars expression | Correct callback/materialization fallback plus warning; no semantic drift. |
| T26 | Conditional branch that would throw if evaluated when not selected | Backend implementation must not execute the unselected branch for affected row. |
| T27 | Target-specific function used without opt-in | Compile failure naming capability and target. |
| T28 | Target-specific function with opt-in | Correct native target code, explicit nonportable manifest entry, failure on other target. |
| T29 | Mixed `Int64`/`Float64` values with possible precision loss | No invisible lossy conversion; diagnostic or explicit cast. |
| T30 | Missing/nullable source value versus NaN | Distinct representations, inspections, group/equality behavior and generated results. |

For data-output comparisons, canonicalize **unordered tables as multisets of typed rows**, not as positional arrays. For ordered views compare their explicitly requested order. Handle null, NaN and float tolerance using the source-language comparison policy. Check schemas, nullability and constraints, not only serialized values.

### 11.1 Minimum benchmark coverage

Adopt the Brown Benchmark for Tabular Types (B2T2) as an external expressiveness/diagnostic benchmark. Publish a datasheet saying which table operators and error cases are supported, partial or unsupported. Do not claim full B2T2 coverage without running its programs and documenting deviations.

### 11.2 Usability test

A competent R/tidyverse user should complete a real analysis exercise: open a contracted table, inspect a nullable field, add a derived variable, correct a name/type error, perform a join, aggregate, inspect an intermediate result and emit R/Python scripts. Record number of explicit annotations, time to discover errors, forced materializations and awkward extra steps. Compare with R's interactive baseline rather than with artificially difficult pandas code.

## 12. Implementation milestones and stop conditions

The following order keeps the language interactive from early prototypes without claiming the whole design is feasible at once. Milestones are slices that must run end to end. Build the hardest mechanism early: typed schema transformations, automatic lifting, and backend-independent semantics.

### M0: syntax and type-system proof of concept

Deliver: parser and formatter for the canonical samples; a type checker supporting primitive values, functions, records, optional values, named table rows and at least basic row polymorphism; an explicit elaboration pass for scalar-to-column lifting and whole-column reducers; a small reference evaluator; diagnostics with source spans. Include failing tests and the beginning of B2T2 coverage.

**Gate:** `standardize(revenue)` and `square(revenue)` are both inferred and elaborated in a principled way; `derive` outputs a correct schema, and `filter` rejects a nullable predicate. If type inference relies on special-case pattern matching for these exact sample functions, M0 fails.

### M1: contracted R vertical slice and usable REPL

Deliver: `use contract`, upstream data-dict `export-spec` and validation integration, a persistent R session, source-to-tidyverse lowering for `filter`, `select`, `derive`, `replace`, `summarize`, `arrange`, `count`, `sum`, `mean`, and basic `join`; terminal `:type`, `:schema`, `:preview`, `:validate`, `:code r`, `:explain`; full contract preflight before materialization; portable script output; simple but functional table inspector.

**Gate:** an analyst can load a Parquet source through a real data-dict file, write and revise a typed pipeline in the REPL, preview it, and generate and execute the same R script without manually rewriting any transformation. Invalid sources fail before exposure of contract-dependent results.

### M2: independent Python/Polars backend

Deliver: Python/Polars lowering, persistent worker, dual-backend code generation, exact/guarded/fallback/unsupported classification, shared reference fixtures, output normalization and performance warnings. Ensure sequential derivations and guarded conditionals work. Package generated scripts with manifests/source maps and required helpers.

**Gate:** the canonical sample and every supported semantic fixture pass against both R and Python. A known source/target disagreement produces a guarded implementation or a compile error, never silent divergence.

### M3: outstanding interactive quality

Deliver: recovery-capable incomplete programs, typed holes, valid-prefix evaluation, revisioned bindings and stale-state handling, schema-driven completion, useful nullability/join diagnostics, fast incremental feedback, rich independent TUI/table inspector, cancellation and bounded preview caching. Add an LSP interface for editors after terminal usability is established.

**Gate:** while editing a broken `derive`, the user can inspect the valid prefix and previously computed outputs, see type constraints around the hole, and continue without restarting the session. Source/contract changes invalidate exactly the affected proof/cache state.

### M4: broaden core table semantics, harden conformance

Deliver: more selectors, joins and reshaping, key/cardinality refinement propagation, robust type diagnostics, extensive B2T2 reporting, property-based testing, more physical Parquet types and backend version-matrix CI. Add optimizations only when tested against reference semantics.

**Gate:** published conformance tables and benchmark datasheet state both successful and unsupported cases, with no known silent semantic mismatch in the declared portable subset.

### Deferred work

Julia/Tidier.jl target; general rank-polymorphic arrays; a marimo-like reactive notebook; plotting grammar and backends; modeling grammar and estimators; general recursive functions if not already present; broad foreign-function support; sophisticated optimizer; deployment/package publishing. These are explicit expansion paths, not hidden M1 requirements.

### Recommended performance budgets (targets, not semantic claims)

Aim for sub-200 ms incremental type/schema feedback for typical interactive edits on moderate modules (excluding data scans), responsive terminal completion, and asynchronous/cancellable previews. Measure instead of guaranteeing latency independently of program size and disk/network conditions. Type inspection must work offline once contract schema metadata is cached. Unexpected full dataset scans initiated by type inspection are bugs.

## 13. Decisions the agent must not improvise

1. **No implicit row order.** `arrange`/explicit ordered windows are the only routes to order-sensitive semantics.
2. **No positional dataframe access.** Names and named ranges only.
3. **One call syntax.** `f(a, b, name = c)` everywhere; no bespoke dplyr-like parser for individual verbs.
4. **`derive` means sequential additions.** Existing-name replacement uses `replace`.
5. **Column scope is explicit and stable.** Bare names in value position prefer fields; `^` is lexical, `$` is column, with collision diagnostics.
6. **Strict Option types.** Null is not NaN and missing values are not automatically filled.
7. **Automatic lifting exists.** It is a type/shape elaboration pass, not a target-specific runtime trick. Ordinary `List<T>` uses `map`.
8. **No unvalidated source facts masquerade as proven.** Runtime data validation is a dependency of contracted materialization.
9. **Use upstream data-dict.** Import JSON exports and validation reports instead of cloning a subtly different validator in the compiler.
10. **No silent backend drift.** Exact/guarded/fallback or fail. Backend escapes are explicit and tracked.
11. **The terminal REPL stands alone.** It includes live types, rich table inspection and useful partial-code behavior.
12. **No plotting/modeling implementation in v0.1.** Reserve typed extension points, then test them after tabular semantics stabilize.
13. **Scripts are the normal output.** Packages are possible later; the native script must run independently of the source compiler with documented dependencies.
14. **A compiled script and the REPL share semantics.** REPL-only special behavior may change presentation or latency but not meaning.
15. **Future optimization is safe by construction.** Data validation, error behavior, group semantics and explicit order are not optional optimizer barriers.

When an implementation problem forces a choice not settled here, add a short Architecture Decision Record (ADR) covering alternatives, the reason for the choice, its semantic effect, and corresponding conformance tests. Prefer the smallest compatible extension. A performance shortcut is not grounds for changing language semantics.

## 14. Known hard problems and safe initial policies

These risks are real; do not bury them beneath a nice parser demonstration.

**Rank inference plus polymorphism.** General automatic rank polymorphism combined with row types and overloaded arithmetic is research-level work. Restrict v0.1 to scalar/column lifting with a deterministic type-directed elaboration algorithm. Emit ambiguity errors where no unique interpretation exists. Defer full Julia-style arbitrary-dimensional broadcasting.

**R/Python semantic differences.** Int64 precision, NaN/None distinctions, categorical ordering, null comparisons, conditional evaluation and empty folds differ across ecosystems. Test these explicitly. Generate guards/helpers or mark a lowering unsupported; never reinterpret the source quietly.

**Whole-column functions in Polars.** `map_batches` does not automatically preserve whole-dataset reduction semantics. Native expressions, window lowerings or explicit full materialization are required when function behavior depends on the entire group/column.

**Proofs over mutable data.** Contract validation on a file that changes during execution does not justify claims about another snapshot. Version or materialize input data and invalidate proofs when inputs change. Do not conflate checking a contract's YAML with checking its data.

**Partially written programs.** Type holes permit local information and valid-prefix evaluation; they do not license evaluating unknown code. It is acceptable for a hole's exact type to remain unknown until more code is supplied, provided the UI reports this honestly.

**Schema-dependent language services.** The compiler must distinguish stable contract-declared column names from schema inferred from a single preview. An incomplete schema reduces static expressiveness; it never produces fake field proofs.

**Idiomatic output versus fidelity.** Sometimes faithful translation needs extra steps. Prefer readable guarded code to a one-line native call that changes results. A script with a small helper can still be idiomatic enough to maintain.

## 15. Definition of done for the first serious release

The initial release is ready for external use only when:

- The documented grammar parses and formats the canonical program; type annotations, ADTs, records, pattern matching and pure ordinary functions work across multiple examples.
- Row-polymorphic named table transformations and automatic scalar/column lifting pass positive and negative inference tests.
- Contracted Parquet sources are imported using data-dict's actual interfaces, with appropriate metadata and required value-validation behavior.
- Nullable values and aggregate corner cases behave according to the documented reference semantics.
- A standalone terminal REPL offers schema/type inspection, rich previews, meaningful diagnostics, source revisions and useful incomplete-program recovery.
- The same checked source produces readable, runnable R/tidyverse and Python/Polars scripts with source maps, dependency manifests and warnings for fallbacks.
- Both outputs pass reference/differential tests for the declared portable subset; unsupported operations yield diagnostics rather than incorrect code.
- There is a B2T2-style report describing implemented operations, known gaps and error quality.
- A user can complete the canonical exploration-to-script workflow without editing generated native code or restarting the REPL after normal syntax mistakes.

A partial implementation should say which gate it satisfies, which it does not, and which tests demonstrate the boundary. Do not label it "complete" merely because a demo script runs.

## 16. Research and authoritative references

References inform design but are not substitutes for the explicit normative choices in this brief. Check current upstream versions while implementing; record any changes to the pinned contracts or backend APIs.

1. **data-dict specification:** https://data-dict.tidyverse.org/spec.html — authoritative YAML format, columns, measures, constraints, relationships, definitions, Parquet source scope.
2. **data-dict validation overview:** https://data-dict.tidyverse.org/validate.html — specification, metadata and full-data validation distinctions.
3. **data-dict validator check codes:** https://data-dict.tidyverse.org/validation.html — check-specific semantics and unverified cases.
4. **data-dict exports:** https://data-dict.tidyverse.org/export.html — resolved JSON export (`export-spec`) and data profiles.
5. **data-dict machine-readable reports:** https://data-dict.tidyverse.org/report.html — stable check codes and outcomes.
6. **data-dict expression language:** https://data-dict.tidyverse.org/expressions.html — scalar/row/aggregate shapes, type rules and functions.
7. **data-dict execution and fidelity:** https://data-dict.tidyverse.org/expression-execution.html — reference semantics and translation classifications.
8. **Hadley Wickham, *Tidy Data* (2014):** https://www.jstatsoft.org/article/view/v059i10 — analytic table representation and manipulation verbs.
9. **Paszke and Xie, *Infix-Extensible Record Types for Tabular Data* (2023):** https://xnning.github.io/papers/tyde23.pdf — row and label polymorphism for tables.
10. **Toohey et al., *Extensible Data Types with Ad-Hoc Polymorphism* (POPL 2026):** https://doi.org/10.1145/3776662 — extensible rows and qualified constraints.
11. **Schenck et al., *AUTOMAP: Inferring Rank-Polymorphic Function Applications* (OOPSLA 2024):** https://doi.org/10.1145/3689774 — inferred maps and scalar replication.
12. **Brown Benchmark for Tabular Types:** https://github.com/brownplt/B2T2 — required expressiveness and diagnostics benchmark.
13. **Hazel, typed holes and live programming:** https://hazel.org/ — principled incomplete-program reasoning and live typed tables.
14. **marimo, reactive notebook semantics:** https://docs.marimo.io/guides/reactivity/ — dependency DAG, invalidation and stale execution.
15. **Yang et al., *A Grammar of Data Analysis* (2025):** https://arxiv.org/abs/2511.02156 — metrics and dimensions as higher-level analysis concepts.
16. **Polars native expressions and callback caveats:** https://docs.pola.rs/api/python/stable/reference/expressions/api/polars.map_batches.html and https://docs.pola.rs/api/python/stable/reference/expressions/api/polars.when.html — fallback and conditional-evaluation constraints.
17. **Wickham, *ggplot2: Elegant Graphics for Data Analysis* / online book:** https://ggplot2-book.org/ — future graphics grammar, not v0.1 implementation scope.

---

**Implementation directive:** Begin with the canonical source file, formalize the type/shape elaboration rules as tests, and build the smallest interactive end-to-end implementation through upstream data-dict and R. Prove backend independence by adding Python/Polars against the same reference fixtures. Treat REPL quality, contract validity and honest cross-backend semantics as acceptance criteria, not stretch goals.
