# Lean coverage of the Rupam specification

The [language specification](../spec/README.md) defines the rules. Each row below
identifies a checked fragment and the next claim that needs a definition or
proof. The [roadmap](formalization.md) orders the work by proof dependency.

The [Specification module](../../formal/Rupam/Specification.lean) states
inference soundness, exact contextual requirements, elaboration equivalence,
and translation preservation as Lean propositions over independently
supplied relations. It proves coherence from elaboration equivalence and
shows how source safety transfers through Core to every admitted target
execution. Instantiating those propositions for all Rupam programs is open.

| Specification | Checked fragment | Open obligation |
| --- | --- | --- |
| [Syntax](../spec/syntax.md) | `OrdinaryResolution` proves lexical resolution for the ordinary function grammar. `Patterns`, `IntegerPatterns`, and `ScopedMatch` cover typed patterns, intervals, and ordered guarded arms. | Connect the full token and grammar judgments to name resolution, typed patterns, and exhaustiveness checking. |
| [Types](../spec/types.md) | `OrdinaryInference`, `Schemes`, and `Specialisation` cover separate parts of polymorphic inference. `ContextInference`, `Roles`, and `SourceSemantics` cover checked contextual calls and their elaboration. `Alignment`, `Lifting`, and `Elaboration` cover scalar broadcasting, checked alignment, and pointwise integer addition. | Prove soundness and coherence for a single source judgment with generalization, inferred parameter roles, numeric promotion and defaulting, and lifting only at parameters that require it. Model all value types and the runtime check for unknown vector lengths. |
| [Missing values](../spec/missing.md) | `Policy` checks missing-policy decisions. `Core` and `Guarded` model optional arithmetic, three-valued Boolean operators, and positional demand for guarded computations. | Show that all source decision points select the specified policy and that propagation, skipping, and failure agree through nested calls. |
| [Tables](../spec/tables.md) | `SchemaRequirements`, `Filter`, `Mutation`, `Grouping`, and `Selection` model parts of schema checking, row preservation, grouping, and selection. `AnalyticalPipeline` and `LinkedMatch` prove deferred-linking equivalence through checked stages and guarded match arms. | Establish whole-verb laws for joins, reshaping, expansion, windows, summaries, and all schema and order transitions. Specify and prove effects for table input and output. |
| [Standard library](../spec/stdlib.md) | `Core`, `ExactPromotion`, `Restriction`, and `Selection` prove laws for selected operations and the scopes at which they run. | Give each primitive a portable denotation and prove result type, missingness, numerical behavior, and demand, including date, time, and statistical operations. |
| [Contracts](../spec/contracts.md) and [Projects](../spec/projects.md) | `Evidence` gives scoped evidence and a conditional check-elimination result. | Prove that validation establishes contract predicates on the locked contents, and that any changed dependency invalidates proofs that depend on it. |
| [Overview](../spec/overview.md), [Session](../spec/session.md), [Tooling](../spec/tooling.md), and [Agent interface](../spec/agent-interface.md) | The checked expression fragments model values and calculation failures. | Define effects and session transitions, then prove source-order execution, failure atomicity, cancellation and revision isolation, and the preview and script agreement promised for locked executions. |
| [Dialects](../spec/dialects.md) and [Runtime](../spec/runtime.md) | `Specification.Preserves` states the preservation obligation on admitted observations. | Model the observable behavior of R and Python lowering, the Arrow and adapter boundaries, and the numeric and effect variation each target permits. Prove preservation for lowering or the soundness of a per-compilation validator. |
| [Guarantees](../spec/guarantees.md) | The model proves fragment-specific invariants, including alignment, portable integer checks, guard locality, and scoped deferred substitution. | Derive each guarantee for the complete source and Core judgments, including the correspondence with execution on locked inputs. |
| [Conformance](../spec/conformance.md), [Development](../spec/development.md), and [Performance](../spec/performance.md) | Lean checks selected shared fixtures alongside general proofs. | Compare all implemented rules across targets. Measure performance claims under the specified workload and environment. |
| [Notebook](../spec/notebook.md) and [Future](../spec/future.md) | Deferred features. | Model the features and prove their interaction with existing rules when they enter the language. |

## Guarded matching

`StagePatternArm.elimination_preserves_match` applies to any finite exhaustive
list of checked arms. Each arm can construct a stage plan from the bindings at
the rows that reach it. The guard receives the rows selected by its pattern,
while the body receives the rows that also pass the guard. Both retain the
enclosing evaluation scope. Direct lookup and selective linking give equal
scoped calculations, so running either at any mask gives the same values,
missingness, row alignment, and calculation failures. Each plan assumes
schema readiness and supplied contextual requirements.

## Proof status

An open obligation can have a Lean statement before a proof. A theorem that
uses it takes the proposition as an explicit premise. The strict build
checks all claimed proofs, and the axiom audit checks the imported Rupam
declarations. Fixture checks exercise the model and help detect drift after
spec changes.
