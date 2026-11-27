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
| [Types](../spec/types.md) | `Application` proves selective lifting at arbitrary arity. `PointwiseSource` checks concrete pointwise specializations. `VectorCalls` checks runtime lengths for prechecked layouts. `QualifiedInference` now gives pointwise and contextual application one declarative source judgment. `QualifiedSchemes` proves principality of generated residual schemes in that fragment. | Normalize and solve the residual constraints. Extend the unified source judgment to multi-argument calls, numeric promotion and defaulting, unknown lengths, inferred roles and stage requirements, transforms, and the remaining value types. Prove coherence of complete source derivations. |
| [Missing values](../spec/missing.md) | `Policy` checks missing-policy decisions. `Core` and `Guarded` model optional arithmetic, three-valued Boolean operators, and positional demand for guarded computations. | Show that all source decision points select the specified policy and that propagation, skipping, and failure agree through nested calls. |
| [Tables](../spec/tables.md) | `SchemaRequirements`, `Filter`, `Mutation`, `Grouping`, and `Selection` model parts of schema checking, row preservation, grouping, and selection. `AnalyticalPipeline` and `LinkedMatch` prove deferred-linking equivalence through checked stages and guarded match arms. | Establish whole-verb laws for joins, reshaping, expansion, windows, summaries, and all schema and order transitions. Specify and prove effects for table input and output. |
| [Standard library](../spec/stdlib.md) | `Core`, `ExactPromotion`, `Restriction`, and `Selection` prove laws for selected operations and the scopes at which they run. | Give each primitive a portable denotation and prove result type, missingness, numerical behavior, and demand, including date, time, and statistical operations. |
| [Contracts](../spec/contracts.md) and [Projects](../spec/projects.md) | `Evidence` gives scoped evidence and a conditional check-elimination result. | Prove that validation establishes contract predicates on the locked contents, and that any changed dependency invalidates proofs that depend on it. |
| [Overview](../spec/overview.md), [Session](../spec/session.md), [Tooling](../spec/tooling.md), and [Agent interface](../spec/agent-interface.md) | The checked expression fragments model values and calculation failures. | Define effects and session transitions, then prove source-order execution, failure atomicity, cancellation and revision isolation, and the preview and script agreement promised for locked executions. |
| [Dialects](../spec/dialects.md) and [Runtime](../spec/runtime.md) | `Specification.Preserves` states the preservation obligation on admitted observations. | Model the observable behavior of R and Python lowering, the Arrow and adapter boundaries, and the numeric and effect variation each target permits. Prove preservation for lowering or the soundness of a per-compilation validator. |
| [Guarantees](../spec/guarantees.md) | The model proves fragment-specific invariants, including alignment, portable integer checks, guard locality, and scoped deferred substitution. | Derive each guarantee for the complete source and Core judgments, including the correspondence with execution on locked inputs. |
| [Conformance](../spec/conformance.md), [Development](../spec/development.md), and [Performance](../spec/performance.md) | Lean checks selected shared fixtures alongside general proofs. | Compare all implemented rules across targets. Measure performance claims under the specified workload and environment. |
| [Notebook](../spec/notebook.md) and [Future](../spec/future.md) | Deferred features. | Model the features and prove their interaction with existing rules when they enter the language. |

## Path to principal qualified schemes

Start with a rank-1 fragment of resolved pure helpers with let generalization,
numeric primitives, presence, and shape-directed application. State a
declarative typing judgment independently of the inference algorithm. At a
fixed lexical environment `Γ`, an inferred scheme `∀α. Q ⇒ τ` is principal
when it is derivable and every other admissible typing `P | Γ ⊢ e : υ`
factors through a substitution `θ` with `υ = θτ` and `P ⊨ θQ`. The
substitution leaves the variables free in `Γ` fixed. Scheme comparison is
up to renaming and constraint entailment. Prove this for the scheme before
literal defaulting. Jones proves this form of generality for qualified HM.
HM(X) makes the constraint theory a parameter and isolates the conditions
under which inference is principal. Rupam must discharge those conditions
for its own predicates and their interaction.
[Jones, §§5–7](https://jgbm.github.io/eecs762f19/papers/jones.pdf)
· [Odersky, Sulzmann and Wehr, HM(X)](https://onlinelibrary.wiley.com/doi/full/10.1002/%28SICI%291096-9942%28199901/03%295%3A1%3C35%3A%3AAID-TAPO4%3E3.0.CO%3B2-4)
· [Pottier and Rémy, §§1.3–1.8](https://cambium.inria.fr/~remy/attapl/emlti-long.pdf)

1. **Infer the scalar signature once.** Give `square` a candidate scheme
   `∀a b. Mul(a, a, b) ⇒ a → b`, with `a` and `b` ranging over cell types
   including presence. Define `Mul` by the numeric and presence rules and
   make its evidence identify the operation and any operand coercions.
   Execution of that evidence must implement the specified overflow,
   rounding, and missingness. At application, generate a passing constraint
   per argument and align the shapes contributed by the chosen modes.
   An exact match contributes no iteration shape even for a collection.
   Known lengths participate in static alignment. Unknown lengths produce a
   check in the elaborated call. Keep unresolved type, presence, and shape
   constraints in the qualified scheme with fresh variables at each use.
   Define entailment by the solutions of these predicates and prove that it
   respects substitution. For each solver rule, prove that the original and
   residual constraints have the same solutions after substitution and
   projection of fresh variables. Prove that every admitted typing factors
   through the residual scheme, including when constraints share variables
   across numeric, presence, and shape rules.

2. **Add latent requirements and per-parameter uses.** Track whole-context
   obligations such as group scope, order, and unresolved missing policies,
   and track each parameter's role and consumption sites separately.
   Petricek's flat systems permit choices about what a closure captures
   versus what its caller supplies. His implicit-parameter example fixes
   this choice using lexical availability. The structural system ties
   requirements to individual variables and uses syntax-directed rules to
   fix the order of structural operations. For Rupam, derive capture and
   discharge from the specified lexical bindings, policy priority, and
   consuming stage. Then show that the same quantified requirement `c`
   appears on the input function and the returned function of the curried
   `apply` scheme `∀a b c. (a →[c] b) → (a →[c] b)`. Prove that
   instantiation, forwarding, and composition preserve the caller's
   obligation and each parameter's consumption sites. The thesis's inference
   prototype establishes no general completeness result, so use its
   syntax-directed method to design Rupam's rules and prove generality here.
   [Petricek, Orchard and Mycroft, ICALP 2013 §§2, 4](https://tomasp.net/academic/papers/coeffects/coeffects-icalp.pdf)
   · [ICFP 2014 §§3–5](https://www.doc.ic.ac.uk/~dorchard/publ/coeffects-icfp14.pdf)
   · [Petricek, thesis §§4.3, 6.3, 7.1, 8.1](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-906.pdf)

3. **Prove coherence at each extension.** For the same resolved source,
   instantiated interface, and supplied context, every admitted elaboration
   must agree on values, alignment, missingness, demand, and the first
   observable failure. Specify exact-match passing before element lifting
   and derive whole-group consumption from the callee's constraints.
   Where two admissible solutions would change the observation, the
   declarative rules must resolve the choice or report ambiguity. Choosing
   the first solver result establishes determinism alone. Check `square`
   at scalar, optional, vector, and column uses, and compare
   `apply(square, x)` with `apply(center, x)` in a group. They exercise
   distinct contextual requirements that a shared result type can conceal.
   [Jones, §7.1](https://jgbm.github.io/eecs762f19/papers/jones.pdf)
   · [Petricek, thesis §§4.3, 5.2.6, 6.3](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-906.pdf)

4. **Add rows and local evidence in separate proof steps.** Unify open
   record rows and record deferred field requirements at their consuming
   stages. Preserve resolved lexical names throughout. Track guard and
   stage assumptions as local givens, and generalize only admissible
   predicates whose variables can be quantified under the lexical
   environment. Keep concrete snapshot evidence tied to its identity and
   revision. For each extension, prove that normalization and scoped
   entailment still admit a principal residual scheme or identify a
   counterexample requiring a narrower generalization rule or annotation.
   OutsideIn(X) shows why unrestricted local assumptions and
   generalization can defeat principality in a given constraint language.
   [Gaster and Jones, row polymorphism](https://web.cecs.pdx.edu/~mpj/pubs/polyrec.html)
   · [OutsideIn(X), §§2, 4, 7](https://simon.peytonjones.org/assets/pdfs/outsideinx.pdf)

Track five independent claims. **Soundness** constructs a valid elaboration
for every accepted program. **Completeness** accepts every declaratively
typable program in the stated fragment. **Principality** gives the
factorization property above. **Coherence** relates the observations of
admissible elaborations. **Decidability** requires a total solver for the
stated domain, including termination of normalization and entailment.
The finite call checker establishes acceptance completeness for concrete
specializations. The qualified judgment below proves principality in a
residual constraint language. A normalizer that solves the residual
constraints and infers a compact scheme is still open.

## Unified qualified inference

`QualifiedSource` covers resolved references, materialized literals,
unannotated unary lambdas, let generalization for syntactic values, scalar
integer multiplication with optionality, and already checked contextual
primitives. The same application judgment dispatches a pointwise function on
materialized data through checked lifting, maps it over a scoped calculation
at demanded positions, or invokes a contextual closure with its full checked
argument and latent requirements. `QualifiedApplies.coherent` proves
uniqueness of application evidence at fixed concrete operand types.
`QualifiedApplies.infer_rejects_iff` characterizes rejection by the absence
of a qualified application derivation at those types.
`QualifiedApplication.run_requires_exactly` checks all and only the selected
callee's latent requirements before execution.

`Qualifies.satisfies` and `QualifiedSource.generation_sound` prove equivalence
between declarative typing and satisfaction of generated constraints.
`QualifiedSource.residual_principal` proves that every sound symbolic scheme
in this constraint language factors through the generated scheme by a
sort-preserving substitution that fixes the lexical variables. The residual
scheme quantifies the type, immediate requirements, and pointwise eligibility.
Its constraints retain existential witnesses for internal interfaces.
`QualifiedSource.verify_exists_iff` proves that a concrete interface has a
valid certificate exactly when it is typable. Certificate replay terminates
and rechecks each constraint. `Qualification.reduceFacts_preserves` proves
that checking ground atoms and simplifying conjunctions preserves solutions.
The generated residual scheme applies this simplification. Solving the
remaining existential constraints is open.

`square_scheme` reduces the generated scheme for `square` to integer operand
compatibility. `forwarder_scheme` reduces the scheme for curried `apply` to the
application predicate. `forwarder_retains_context` specializes that predicate
to a contextual closure with the same latent requirements on its input and
output arrows. `forwarder_retains_pointwise` gives the specialization that
preserves the pointwise interface, including whole collection parameters.
The composed source proofs send a lifted vector or scoped
calculation through that forwarder to a checked contextual operation.
`ContextTerm.runQualifiedApplication_eq` preserves existing contextual
execution under the shared call rule. `QualifiedApplies.run_lifted_reduction`
proves that a demanded reduction reads the lifted inputs across the full
enclosing scope and broadcasts to the demanded positions, preserving
failures. `run_contextual_inactive` covers empty demand for a scoped result.
`ContextElaborates.linked_qualified_lifting`
preserves observations after selective linking of a scoped argument under
the existing schema-readiness premise.

`forwarder_lifting_placements` exhibits two admitted interfaces for passing
a pointwise function through `apply`. One lifts at the call to the returned
closure. The other gives that closure a vector parameter and lifts inside
its body. Both can accept the same vector and produce the same result type.
The full source rules must fix the passing role or establish observational
equivalence of the two placements, including demand and failure. A checked
negative example squares `[1, intMax]`. Demanding only the first cell
succeeds, while evaluating the whole result before selection reports
overflow. A proof that changes the placement must preserve demand.

The proofs start after name resolution and take contextual primitive types
and their role information from checked terms. Contextual role inference,
runtime-sized layouts, and the rest of source elaboration still need to
enter the unified judgment. The principality theorem covers the declarative
fragment above and its residual constraint language.

## Checked pointwise calls

`PointwiseFunction.inferCall` takes a resolved helper body with unannotated
parameter slots and a list of source argument expressions. It checks the
declared arity and argument types, then tries whole and element types at
each parameter slot, with whole first. `inferCall_succeeds_iff` proves it
finds a specialization exactly when one has a source typing and checked
alignment. `inferCall_prefers_whole` proves that an entirely whole
specialization takes precedence when it checks. This finite reference search
can try exponentially many specializations. The body fragment includes
required and optional integer addition and multiplication, required Boolean
guards with a common branch type, literals, parameter references, and
application of a supplied pointwise function value at its declared input type.
`InferredPointwiseCall.run_preserves_source` proves that every accepted call
runs like its source typing derivation at any ordered list of demanded
positions. This includes eager argument materialization, guarded body
evaluation, and failures. C165 checks the resolved syntax of
`square([1, 2, 3])` and vector addition. Parser integration and generalization
of helper definitions remain open.

`CheckedCall.parameter_whole_iff_match` characterizes whole passing by exact
type equality. `CheckedCall.parameter_of_match` proves that the parameter
receives the original value at every result position, including when the
value is a vector or column. `ArgumentPassing` permits element-wise
lifting when the collection's element type matches the parameter and the
collection type differs. Only these lifted arguments contribute a collection
shape to alignment. `CheckedCall.infer_rejects_iff` characterizes rejection by
the absence of a passing and alignment derivation for the supplied type lists.
Alignment uses known vector lengths and column domain identities.

`CheckedCall.run_eq` applies to any finite argument list and any ordered list
of demanded result positions. It equates lowered Core execution with scalar
body evaluation on whole values and aligned elements. The equality preserves
values, missingness, and the first observed failure. Ordinary argument
evaluation proceeds in source order even for empty demand. The body runs
only at demanded positions and retains its scalar guard and short-circuit
semantics. `CheckedCall.run_demanded_failure_iff` identifies the first failing
position in the demand list once eager argument evaluation succeeds. The
theorems start from checked pointwise bodies and supplied argument and
parameter types.

`CheckedVectorCall.infer` checks the statically known lifted lengths.
`VectorCallValues.align_rejects_iff` proves that after eager materialization,
alignment fails exactly when the lifted vectors lack a common length.
`CheckedVectorCall.statically_sized_align` proves accepted static layouts
stay aligned when every lifted length is known. The runtime checker uses a
distinct length-mismatch failure even when the demand list is empty.
`CheckedVectorCall.run_argument_failure` proves that a materialization
failure takes precedence over runtime alignment and body evaluation.
`CheckedVectorCall.run_preserves_source` covers checked vector layouts and
a typed pointwise body. Inference of those layouts from unresolved source
shapes remains open.

`ContextTerm.forward_lifted_call` also connects checked pointwise results to
the contextual higher-order calculus. Forwarding a lifted calculation to an
already typed contextual function preserves the supplied latent requirements
and the calculation's meaning at the enclosing scope.

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
