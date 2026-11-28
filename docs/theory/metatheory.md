# Metatheory

| Property | Statement | Status |
| --- | --- | --- |
| Type preservation | evaluation preserves types, shapes and domains | checked pointwise terms carry their result type and shape in Lean. General preservation remains a conjecture |
| Domain safety | incompatible observations cannot combine implicitly | proved for column alignment, equal-length vector alignment, and the separation of vectors and columns |
| Lifting | a lifted pointwise helper agrees with its scalar body at each aligned position and receives exact-type arguments whole | proved for checked calls and connected to inference for resolved, unannotated first-order helpers with integer arithmetic and Boolean guards. Grouped calls are modeled separately |
| Principality | every sound scheme is an instance of the inferred scheme under the same lexical environment | proved for residual schemes in the shared qualified fragment with unary pointwise and contextual application and let-bound values. Normalization and the full source judgment remain open |
| Context safety | requirements are discharged at consumption sites | proved for checked contextual terms with explicit scope and policy requirements. Source inference remains open |
| Source elaboration | accepted source has a checked elaboration and a coherent meaning | proved for the modeled scalar and column expressions and the ordinary lambda fragment |
| Deferred argument linking | resolving deferred columns at each consuming stage preserves execution | proved for finite checked pipelines in the contextual source fragment |
| Extraction equivalence | extracting an expression into a helper preserves meaning | proved for inlining arguments with no immediate requirements in checked contextual terms. General extraction remains open |
| Grouping stability | mutation cannot invalidate an established grouping witness | conjecture |
| Guard safety | inactive computations cannot fail | proved for the modeled branch-selection rules, guarded reduction and grouped coalescing. General safety remains a conjecture |
| Pattern matching | the first applicable arm sees its selected positions | proved for the modeled patterns, pointwise guards, and scoped arm bodies |
| Refinement soundness | inferred predicates hold for evaluated values | conjecture |
| Evidence locality | snapshot evidence cannot escape its scope | conjecture |
| Backend preservation | compiled programs preserve the specified observable semantics | evaluated by conformance and differential tests. General preservation remains a conjecture |

Extraction equivalence distinguishes Rupam from dataframe languages with
nonstandard evaluation: a reusable helper means what its inline body means.

## Lean development

The [Lean core](../../formal/Rupam/Core.lean) models required and optional
Bool and Int values, scalar broadcasting, aligned vector and column
arithmetic, and pointwise guards. A column's domain includes its identity
independently of its length. Integer operations return a portable integer or an
explicit failure. Integer summation is invariant under permutation and checks
the exact final sum. A guarded reduction receives the entire group and
broadcasts one result to the selected rows, or skips the reduction when the
selection is empty. [Grouped coalescing](../../formal/Rupam/Grouped.lean)
evaluates the left operand on active rows. One missing value demands the
fallback on the entire group. Successful evaluation preserves the active rows
and their order. [Policy resolution](../../formal/Rupam/Policy.lean) models
how a single decision point captures its lexical policy and how a consuming
site fills an unresolved requirement.

The [lifting module](../../formal/Rupam/Lifting.lean) translates a checked
scalar helper body to Core at a chosen shape. Its proof compares the lifted
body at one position with a direct scalar evaluation on the inputs at that
position. The equality includes missing results and arithmetic failures.
The [scope module](../../formal/Rupam/Scope.lean) fixes the vector or group
and puts the active positions in the result type. Branches evaluate on the
positions that select them. A demanded reduction or transform evaluates
its input over the entire collection, then broadcasts its scalar result or
returns the value at each selected position. The same rule applies if the
input contains another reduction.

The [application module](../../formal/Rupam/Application.lean) composes these
rules for demanded first-order calls over checked arguments. Arguments
evaluate in order, with the scope of each argument preserved. The helper
receives their values at each position. An argument can fail even when
the body ignores it. The checked higher-order calculus carries each
callee's latent requirements through application. The substitution proof
preserves those requirements and lexical bindings. [Pointwise source inference](../../formal/Rupam/PointwiseSource.lean)
infers parameter types from the resolved helper body and source arguments,
then proves that the resulting call preserves the source body evaluation at
each demanded position. Deferred columns recheck
their named fields against the consuming stage, while captured lexical
values retain their identity. Source argument evaluation and inference of
these roles and requirements remain outside the checked calculus.

[Elaboration](../../formal/Rupam/Elaboration.lean) proves coherence for the
modeled pointwise source expressions. [Ordinary inference](../../formal/Rupam/OrdinaryInference.lean)
checks generated type equations and returns a typed term that erases to the
resolved source. Its [operational semantics](../../formal/Rupam/OrdinarySemantics.lean)
proves that the typed term and resolved source produce the same observation.
Generalising a solved definition and checking an instance preserve its
ordinary lexical context and erased source.

The contextual [source semantics](../../formal/Rupam/SourceSemantics.lean)
interprets the source typing rules directly. [Selective linking](../../formal/Rupam/SelectiveLinking.lean)
checks the schemas required by each stage's statically used arguments. The
usage analysis includes both arms of a guard. The proof shows that unused
bindings cannot affect evaluation or require fields at that stage.
[Pipeline preservation](../../formal/Rupam/AnalyticalPipeline.lean) extends
the equality to finite checked pipelines whose later stages can depend on
earlier results. It preserves the active rows, values, missingness, and
failures of the modeled source calculus.

[Match selection](../../formal/Rupam/MatchSelection.lean) proves that a
successful arm suppresses all later tests.
[Match execution](../../formal/Rupam/MatchExecution.lean) proves that each
arm reads only its selected positions and an unselected arm cannot fail.
The [pattern rules](../../formal/Rupam/Patterns.lean) cover literal,
optional, and product patterns. The [integer range theorem](../../formal/Rupam/IntegerPatterns.lean)
equates overlap detection with the existence of a portable integer in both
ranges.

| Lean theorem | Spec rule |
| --- | --- |
| `Align.columns_same_domain` | Combining columns requires a common observation domain. [Types](../spec/types.md#shapes-and-lifting) |
| `Align.infer_complete`, `Align.infer_rejects_iff` | Pairwise alignment accepts exactly the shape combinations in the alignment judgment. It rejects recycling and implicit vector-to-column alignment. [Types](../spec/types.md#shapes-and-lifting) |
| `ScalarBody.lifting`, `ScalarBody.lower_nested_pointwise`, `ScalarBody.vector_lifting` | Pointwise helper bodies lift element-wise, including composition and nested pointwise guards. Vector collection preserves position order. [Types](../spec/types.md#shapes-and-lifting) |
| `ScopedCalculation.call_at`, `ScopedCalculation.call_argument_failure` | At a demanded call, checked argument computations precede evaluation of the body. [Syntax](../spec/syntax.md#grammar) |
| `ValuesAt.mem_select_iff`, `ScopedCalculation.branch_inactive`, `ScopedCalculation.reduce_broadcast`, `ScopedCalculation.transform_at` | Guards change the active positions. A demanded operation reads its full vector or group and aligns its output to the selected positions. [Types](../spec/types.md#shapes-and-lifting) |
| `sumInt_perm` | Integer summation is exact and checks the final result, independent of order. [Standard library](../spec/stdlib.md#reductions) |
| `PolicyDemand.resolve_sound`, `DischargesPolicy.resolve`, `PolicyDemand.resolve_failure_iff` | A policy stays fixed once captured. An unresolved policy takes the first applicable source, and fails when none is available. [Missing values](../spec/missing.md#scope) |
| `Group.completeCoalesce_when_present`, `Group.completeCoalesce_when_missing`, `Group.coalesce_preserves_rows` | A grouped coalesce demands its fallback only where needed, with the full group available to a demanded reduction. Grouped mutation preserves alignment. [Types](../spec/types.md#shapes-and-lifting) · [Tables](../spec/tables.md#verbs) |

Run `lake build` in `formal/` to check the proofs. From the repository root,
`python formal/check.py` (Python 3.11+) runs C37, C52, C53, C55, C56, C110,
C119, C123, C124, C138, C139 and C165 against the same TOML expectations used by the OCaml reference runner.
C124 passes in Lean and remains pending in the OCaml reference suite.
The Lean cases translate the fixture inputs by hand. The check also compiles
a Lean example for C111, which remains pending in the OCaml reference suite.

The Lean workflow runs on changes under `formal/` or to the workflow file.
The local and CI checks use the pinned toolchain, treat warnings as errors,
and reject dependencies on axioms outside Lean's standard foundational axioms.

## From language soundness to compiled correctness

Prove language properties about the specified semantics. Use shared
fixtures to check that the executable Lean fragment and the OCaml reference
agree, and use differential tests to compare generated R and Python with
the reference on the tested inputs. A claim for every emitted program also
needs a preservation proof for the compiler stages or a sound validator that
accepts each translation under the same input and runtime assumptions.
A property of every allowed source behavior then transfers to the emitted
program if every target behavior corresponds to an allowed source behavior.
[CompCert's preservation theorem](https://compcert.org/man/manual001.html)

The first validation boundary is checked TransformIR before and after a
logical rewrite. The optimiser can supply a certificate, and a small
independent checker can verify its premises and the exact input and output
subtrees. A failed check leaves the original Core in place. A guard rewrite
must preserve which rows can fail and which rows feed a reduction. Alive2
checks refinement between two LLVM IR fragments and bounds loop reasoning
by unrolling. Its approach motivates validating individual rewrites. A
bounded search alone leaves larger groups or tables outside the guarantee.
[Optimiser validation](../spec/future.md#optimiser-validation) · [Alive2, PLDI 2021](https://users.cs.utah.edu/~regehr/alive2-pldi21.pdf)

Backend certification would also need semantics for the supported host
operations and a verified link from the checked host AST to the emitted
program. Runtime versions, Arrow interchange, and the behavior of native
libraries are assumptions until covered by proofs. The relation should
preserve order, missingness, errors and effect traces, and allow only the
floating-point differences the spec permits. The tolerance used in
conformance comparisons alone cannot justify a rewrite that changes a branch
or a discharged obligation. [Dialects](../spec/dialects.md#effects-and-portability)
