# Conformance

Each portable fixture runs the reference evaluator and both dialects; compare
values, schema, optionality, refinements, keys, row order, and diagnostic
categories. Fixtures supply concrete data and expected outcomes independently
of host behavior. Compare unordered tables as multisets of typed rows;
for sorted tables with ties, compare ordered tie blocks as multisets, and
for totally ordered tables compare the complete sequence. Integer and
structural results compare exactly. For finite floating numerical results,
use symmetric relative/absolute tolerances (`rtol = 1e-8`, `atol = 1e-12`
by default, fixture-specific when justified); no tolerance on categorical
results. Numerically unstable branch fixtures test documented permitted
outcomes, not false universal equality. Random and clock fixtures compare
using fixed effect inputs or same-target reproducibility, not cross-target
streams. Foreign fixtures run only in their declared dialect.

Test REPL/script agreement on the same lock, checking full data rather than
only preview rows. Test that failed relocking leaves previous proofs stale
and that generated scripts reject stale inputs. Code-generation tests also
check that proved obligations emit no redundant runtime assertions.

## Required cases

| ID | Program | Expected |
| --- | --- | --- |
| C01 | `let square(x) = x * x; square(3)` | `9 : Int` |
| C02 | `mutate(s = square(revenue))` | element-wise; `s : Float` |
| C03 | `mutate(s = square(cost))`, `cost : Float?` | `s : Float?` |
| C04 | `mutate(s = coalesce(cost, 0.0))` | `s : Float` |
| C05 | `mutate(a = revenue + 1.0, b = a * 2.0)` | `b` uses the new `a` |
| C06 | `mutate(revenue = revenue + 1.0)` | `revenue` replaced in place |
| C07 | `filter(cost > 0.0)` | type error naming `cost`; accepted with `missing = skip` |
| C08 | `select(region:cost)` | inclusive range in schema order |
| C09 | `select(typo)` | unknown column, with suggestions |
| C10 | `let revenue = 3.0` then `mutate(a = revenue)` on a table with `revenue` | ambiguity error; `$revenue` and `^revenue` accepted |
| C11 | `standardize(revenue)` | whole-column; `Float?` |
| C12 | columns of two unrelated tables combined | type error |
| C13 | `summarize(s = sum(x), missing = skip)` on an all-missing group | `None : Float?` |
| C14 | `summarize(n = n())` on an empty table | one row, `n = 0`; with `by =`, zero rows |
| C15 | `left_join` with missing keys and unmatched rows | missing keys never match; right columns optional |
| C16 | join with a duplicated non-key column | type error |
| C17 | `distinct(x, missing = keep)` with several `None` | one `None` row; `n_distinct` counts it once |
| C18 | `arrange(desc(v), id) \|> slice_head(n = 5)` with unique `id` | five largest, ties by `id` |
| C19 | `slice_head(n = 5)` on an unordered table | type error |
| C20 | contracted source with `None` in a required column | validation fails before output |
| C21 | `range: [0, 100]` with a value of 101 | no failure |
| C22 | source changed after validation | stale lock; old script rejects; relock validates and recompile |
| C23 | restricted column | absent from previews, viewer, diagnostics, output |
| C24 | unfinished `mutate(z = fn(` | prefix checks and previews; unfinished stage never runs |
| C25 | `if (x > 0.0) log(x) else None` | type-checks; `log` receives only positive values |
| C26 | user function with `match` lifted over a column | native code, no callback |
| C27 | `mutate(rate = cases / population)` on unknown population; locked population containing zero or all nonzero | pending domain obligation; zero fails; all nonzero accepted on explicit lock; filter or ratio works without snapshot proof |
| C28 | source file with NaN in a float column | read as `None` |
| C29 | source file with infinity in a float column | read fails |
| C30 | `x == 0.3`, `x : Float`; `x ~= 0.3` | exact and approximate comparisons respectively; neither changes with binding extraction |
| C31 | `round(2.5)`, `round(-0.5)` | `2.0`, `-0.0` |
| C32 | `d"2026-01-31" + months(1)` | `d"2026-02-28"` |
| C33 | `DateTime<"UTC">` compared with `DateTime<"Europe/Paris">` | type error |
| C34 | `PatientId` joined against `HospitalId` | type error |
| C35 | `left_join` on a non-key without `relationship` | type error |
| C36 | `1..=0`; `1..=5 \|> rev()`; `1:3` outside a selector | empty vector; `[5, 4, 3, 2, 1]`; `r-range` with fix `1..=3` |
| C37 | `[1, 2, 3] + [1, 2]` | length error |
| C38 | `arrange(name)` on mixed-case strings | byte order |
| C39 | `match bmi { ..<18.5 => Under, 18.5..<25.0 => Healthy, 25.0.. => Over }` on a column | enum column; exhaustive without `_` |
| C40 | `Int(x)` with `x : Float`; `Int(round(x))` | type error; accepted with checked range |

## Interaction and boundary cases

| ID | Program or input | Expected |
| --- | --- | --- |
| C41 | `let inc(x) = x + 1; (inc(2), inc(2.5))` | `(3, 3.5) : (Int, Float)` |
| C42 | `100 * 0.75`; `[1, 2.5]`; `if (TRUE) 1 else 2.5` | `75.0`; `[1.0, 2.5]`; `1.0`, all float elements/results |
| C43 | `let i = 9_007_199_254_740_993; i * 1.0` | precision-loss error; `9_007_199_254_740_992` promotes exactly |
| C44 | maximum portable Int plus one; literal `-9_223_372_036_854_775_808` | overflow; out-of-range literal error |
| C45 | `1e308 * 1e308` | arithmetic-overflow; never infinity or `None` |
| C46 | `let x = 1e-200 * 1e-200; 1.0 / x` | x is zero; divisor refinement not established, type error |
| C47 | `exp(-1000.0)` | `0.0`, no positive refinement |
| C48 | `2 ^ (-3)`, `(-2) ^ 3.0`, `0 ^ 0`, `(-2) ^ 0.5`, `0 ^ (-1)` | `0.125`, `-8.0`, `1.0`, domain type error, domain type error |
| C49 | `Int(round(1e100))` | conversion-range error, not wrap or None |
| C50 | `1.0 ~= 1.000000001` in both operand orders; `0.0 == -0.0` | TRUE in both approximate comparisons; exact TRUE for zero |
| C51 | `x != 0` and `let zero = 0.0; x != zero`, x is `1e-300` | both TRUE, nonzero refinement from exact comparison to known zero |
| C52 | `FALSE and None`, `TRUE or None`, `TRUE and None`, `FALSE or None`, `not None` with Bool? operands | FALSE, TRUE, None, None, None |
| C53 | `x == None`, `x != None`; `some(x)`, `none(x)` on `[1, None]` | comparisons rejected; predicates `[TRUE,FALSE]`, `[FALSE,TRUE]` |
| C54 | filter optional x `[None, -1, 2]` by `x > 0`, `skip` vs `keep` | skip retains `[2]` and present positive type; keep retains `[None,2]` and optional positive type |
| C55 | `if (some(x)) x + 1 else 0`, x is Int? | present Int; missing element becomes 0 |
| C56 | `let empty: [Int] = []; sum(empty)`; `sum([None,None], missing = skip)` with Int? elements; `sum([0,0])` | None, None, 0; nonempty known-present sum is present |
| C57 | grouped mutate x `[2,9,4]`, key `[A,None,A]`, `m = mean(x), y = x + 1, missing = skip` | m `[3.0,None,3.0]`, y `[3,10,5]`; three rows |
| C58 | `standardize([2.0,None,4.0], missing = skip)` | `[-0.70710678,None,0.70710678]` |
| C59 | `let avg(x) = mean(x)` then `summarize(m = avg(x), missing = skip)` for `[1,None,3]` | `2.0`; helper inherits policy |
| C60 | `mean_active(data, x)` filters active then reduces x; call with `revenue - cost`, rows as `(active, revenue, cost)` = `(TRUE,100,60),(FALSE,200,20),(TRUE,300,250)` | `45.0`, not `90.0` or a length error |
| C61 | helper replaces revenue with `revenue * 2` then averages substituted `revenue - cost`, initial `(100,60)` | `140.0`; removed referenced column gives a diagnostic |
| C62 | `let center2(x) = x - mean(x); let f(x) = mean(center2(x))` with x `[2.0,4.0]` | scalar `0.0`; nested reduction stays in group scope |
| C63 | mutate using an independently materialised vector of the same length, or two filtered table domains | reject positional attachment and cross-domain arithmetic |
| C64 | guarded whole conversion of `[None,2.0]`; inactive elements of a well-typed guarded branch | `[0,2]` for `if (some(x)) Int(round(x)) else 0`; no failure from inactive elements |
| C65 | required CustomerId joined to optional CustomerId with `missing = skip`; differing sign refinements | compatible keys; missing never matches; Int/Float key pair rejected |
| C66 | `relationship = one_to_one` with duplicate matching keys | cardinality error before output |
| C67 | left join with unmatched left order, `unmatched = error` | coverage error; unrelated unused right rows alone do not fail |
| C68 | full join left key `customer_id`, right key `id`, right-only required key 3 | coalesced key 3 in customer_id; unmatched non-key columns optional |
| C69 | replace key by constant, then join without relationship; rename key without replacing | first loses key, second preserves it |
| C70 | `distinct(id)` with rows `(1,40),(1,41)` | only id column, one row containing 1 |
| C71 | sort tied date then lag; sort by date and unique visit_id then lag | first fails totality obligation; second succeeds |
| C72 | local `order_by = date` with unique `(patient_id,date)` and `by = patient_id` | lag computed within each group, results realign to original rows |
| C73 | filter after total ordering, project away sort column, then lag | order witness preserved; no redundant sorting |
| C74 | `pivot_wider` duplicate id/level cell; explicit `values_fn = sum` | error without aggregation; sum with aggregation |
| C75 | enum-derived levels Height, Weight, only Height observed | both output columns in declared order; missing Weight cell optional |
| C76 | `pivot_longer` integer and float measurements | common Float with exact checked promotion |
| C77 | `across(starts_with("score_"), standardize)` | statically expanded columns; zero matches is a no-op; collisions rejected |
| C78 | row bind `{id:Int, x:Int}` with `{id:Int, y:Float}` | schema id:Int, x:Int?, y:Float?; missing cells added |
| C79 | `Table([{a = 1}, {a = 2.5}])` | table `{a:Float}`; unordered, no inferred uniqueness solely from construction |
| C80 | complete January and March cases to months `1..=3` | February row created with None; fill changes only new cells by default |
| C81 | `expand` over known enum with an unobserved level and year `2025..=2026` | complete Cartesian product, including unobserved level |
| C82 | required contracted float with NaN; conditional assertion `not active or population > 0` | requiredness fails after normalisation; no unconditional positive refinement |
| C83 | contracted entity keys patients.id and hospitals.id; visits.patient_id foreign key to patients | patient and hospital nominal domains incompatible; visits key shares patient domain |
| C84 | two contract enums with equal labels; explicit shared enum annotation | independent domains initially incompatible; shared validated declaration permits combination |
| C85 | restricted column renamed, copied, aggregated | derived restricted values omitted from viewer, diagnostics and output |
| C86 | malformed numeric CSV field; quoted empty text; unquoted empty field; large exact integer | schema error; present empty string; None; exact integer |
| C87 | `let x = 1; let f() = x; let x = 2; f()` in script and REPL | 1; failed subsequent binding leaves session unchanged |
| C88 | inferred CSV in strict REPL; strict standalone export | exploration succeeds; export needs explicit schema or explicit full-snapshot schema commitment |
| C89 | change source, input data, contract, dependency or compiler fingerprint | each invalidates the lock; old artifact remains bound to original lock and rejects changed runtime inputs |
| C90 | source changed between verification and reading | verified snapshot used or consistency error; never unverified bytes |
| C91 | `set_seed(0)` then random draws in strict mode | no warning; repeated same-target locked run reproduces draws; cross-target stream equality not required |
| C92 | `map([1,2,3], fn(x) => x * 2)`; `reduce([1,2,3],0,fn(a,x) => a+x)` | `[2,4,6]`; 6; ordinary host higher-order lowering permitted |
| C93 | `1_00_000`, `1.234_56`, `1e1_0`; `not x > 0`; `[..rest]` | valid literals, Boolean negation of comparison, valid rest-only pattern |
| C94 | source with unsupported formula or module import | unsupported-feature or parse error, not silent acceptance |
| C95 | nominal Usd used in `log` without explicit unwrapping | type error; explicit base conversion works with positive refinement |
| C96 | a proved key, bounds, promotion or overflow obligation in deterministic locked analysis | no redundant assertion in emitted analysis; lock verification remains |
| C97 | floating reduction `[1e16, 1.0, -1e16]` followed by sign-based branch | document native rounding dependence; do not require cross-dialect discrete equality |
| C98 | foreign declaration in strict mode; non-strict foreign return of incompatible representation | strict rejection; return-boundary error in non-strict mode |

| C99 | `let zero = 0.0; x == zero` versus `x == 0.0`, including signed zero and tiny nonzero | identical exact results and refinements |
| C100 | `set missing = skip; let avg(x) = mean(x)` called in a verb with `missing = keep`, x `[1,None,3]` | lexical skip yields 2.0; helper with no lexical policy inherits propagate and yields None |
| C101 | pass contextual `mean_by` through a higher-order forwarding helper | deferred expression, group scope and policy requirements preserved; mixed incompatible value/column role rejected with annotation or split-parameter diagnostic |
| C102 | helper captures lexical `offset`; an earlier mutation adds column `offset` | captured reference remains lexical; explicit column reference sees added column; a genuinely ambiguous fresh name fails |
| C103 | `` mutate(`missing` = none(x), `by` = region, by = region) `` | output columns named missing and by; grouping uses input region; duplicate decoded output names rejected; formatter preserves required quotes |
| C104 | `summarize(m = mean(x), by = region, missing = skip, group_missing = keep)` with `(None,1),(None,None),(A,3)` | missing-region group retained with mean 1; A mean 3 |
| C105 | grouped mutate with required x, missing grouping key and `group_missing = skip` | group-dependent result optional and excluded row None; row count unchanged |
| C106 | `mutate(across(where(is_number), standardize), by = year)` | implicit selection excludes year; generated expressions read entry snapshot; later arguments see outputs |
| C107 | explicitly mutate grouping column then compute grouped mean | group membership fixed at verb entry |
| C108 | `slice_min(score, n = 2, ties = drop)` with scores `[1,1,2]` then with `[1,2,2]` | first succeeds with sorted tied output; second needs cutoff tie-breaker |
| C109 | validate predicates `[TRUE,FALSE,None,None]`, `[None,None]`, empty under skip | counts 1/1/2 and fraction 0.5; all unknown and empty yield undefined fraction, thresholds not applied |
| C110 | fully inactive conditional branch containing integer sum of `[MAX,1]` | no overflow; if any row selects branch, full-group sum runs and fails |
| C111 | integer sum `[MAX,1,-MAX]` and permutations | exact 1, no intermediate overflow |
| C112 | write/read required empty string and optional None to CSV | quoted empty string stays present; unquoted empty remains missing |
| C113 | select no columns from a three-row table | zero columns, three rows, n() = 3 |
| C114 | random-dependent key or nonzero assertion after successful preview | no snapshot proof; necessary runtime check remains |
| C115 | discover pivot levels B,A in full exploration without explicit levels | provisional A,B schema; explicit lock commits; changed levels require relock and downstream check |
| C116 | source schema discovery accepted by explicit lock | no manual schema declaration needed; inferred schema remains provisional before acceptance |
| C117 | R proves an obligation through floating reduction, Python not evaluated | R proof cannot discharge Python obligation |
| C118 | probe on incomplete pipeline and restricted derived data | completed prefix completion; no unfinished execution, restricted values hidden |
| C119 | `if (flag) mean(x) else 0.0` for group x `[2.0,4.0]`, flag `[TRUE,FALSE]` | `[3.0,0.0]`; full-group reduction, not selected subset |
| C120 | unknown domain in a generic helper instantiated on two different snapshots | successful proof for first cannot justify second; second rechecks |
| C121 | `mutate(group = id, previous = lag(value, order_by = month), by = group)` with unique `id` | `lag` is ordered within the entry groups; uniqueness of the new `group` does not discharge the ordering requirement |
| C122 | `1.0 / sum([1e16, 1.0, -1e16])` under a lock that observed a nonzero sum | the divisor check remains in generated code |
| C123 | `mutate(r = coalesce(x, mean(y)), by = g)` | `mean(y)` runs only in groups with a missing `x`, over all of that group's `y` |
| C124 | `let center(x) = x - mean(x); let apply(f, x) = f(x)` then `mutate(z = apply(center, value), by = group)` | same result as `mutate(z = value - mean(value), by = group)` |
| C125 | `map([1.234, 5.678], round(_, 2))` | `[1.23, 5.68]` |
| C126 | `lookup(customers, customer_id = 7)` on a key, then on a non-key | a `{...}?` row; type error |
| C127 | `match x, y { 0, _ => A, _, 0 => B, _, _ => C }` | exhaustive; selects per row |

## Agent and session cases

| ID | Program or input | Expected |
| --- | --- | --- |
| C128 | `penguins \|> summarize(m = todo, by = species)` | checks; hole reports numeric type, group scope and candidates including `mean(body_mass)`; `run` and `compile` refuse |
| C129 | `filter(cost > 0.0)` with optional `cost` through the session protocol | `optional-condition` with a decision whose options are `missing = skip` and `missing = keep`, each with its schema consequence |
| C130 | `select(body_mas)` | `unknown-column` with a fix to `body_mass` |
| C131 | `mean(x, na.rm = TRUE)`; `t %>% filter(x > 1)`; `NA` | `r-na-rm` decision; `r-pipe` fix; `r-na` fix |
| C132 | `define` an item, redefine a dependency | dependant reported stale; old revision still served to an open result handle |
| C133 | evaluate `write_csv` without passing its effect set | rejected; nothing written |
| C134 | `page` of a table with a restricted column at `rows` exposure | column absent; omission reported |
| C135 | `fork`, define in the fork, `context` on both | original session unchanged |
| C136 | `save` after defines, replacements and a removal | canonical `.rp` in dependency order that checks to the same schemas |
| C137 | helper annotated `(t: Table<{a: Int, b: String}>)` applied to a table whose columns are `b, a` | accepted; the result keeps the order `b, a` |

| C138 | `n_distinct([1, 2, None, None])` under `keep` and `skip` | 3 and 2, both `Int` |
| C139 | `first`/`last` of `[None, 10, 20]` and `[10, 20, None]` under `keep` and `skip` | first: `None`/10 under keep, 10/10 under skip; last: 20/`None` under keep, 20/20 under skip |
| C140 | `elevation` (four ranges over `[1, 3000]`) applied to a contract-bounded column, then to an unbounded one | first checks; second is `nonexhaustive-match` naming the uncovered intervals |
| C141 | `income ?? income2 ?? 0.0` with `income2` failing where `income` is present | no failure; result `Float`; `revenue ?? 0.0 > 1.0` parses as `(revenue ?? 0.0) > 1.0` |
| C142 | `x not in ys` and `not (x in ys)` over optional `x`, empty `ys` and `ys` containing `None` | identical results |
| C143 | `match? score { 90.. => "A", ..<90 => "B" }` over `[95, None, 10]` | `["A", None, "B"]`, type `String?` |
| C144 | `:target python-polars` after a `write` in an R session | no rewrite of the file; R-specific evidence discarded; values re-established only on demand |
| C145 | pipeline with `!view()` and `!hist(revenue)` taps, then `:save` | taps run once in pipeline order, leave the value unchanged and are absent from saved source |

## Additional implementation requirements

The interaction cases above are normative acceptance criteria, not results of
executed tests. The typed-core inference rules described in Types must be
specified as a minimal calculus and tested before implementing the full
standard library. Add concrete fixtures for the supported role annotations,
constraint solving and generalisation, policy capture and call instantiation.
Include negative cases and confirm that a proof derived from one target or
snapshot is never reused for a different one without valid shared evidence.

### Live viewer interaction

Open a table and edit a Rupam filter without pressing Enter. Compare the full result and logical schema against the identical REPL expression and compiled script on the same snapshot. Leave a trailing pipe, misspell a column and correct it. Keep the previous grid labelled until the correction succeeds. Deliver a late reply and verify it cannot overwrite current rows or diagnostics. Verify an effectful draft performs no write, scrolling performs no evaluation, and inserting the filter at the prompt preserves its source.

## Benchmarks

The [Brown Benchmark for Table Types](https://github.com/brownplt/B2T2), with
coverage reported in [the B2T2 datasheet](../b2t2.md).
