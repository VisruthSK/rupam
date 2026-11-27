# Formalizing Rupam

The [language spec](../spec/README.md) defines the semantics. Lean checks the
model and proofs. The [metatheory register](metatheory.md) records which
theorems hold today and states the fragment each theorem covers.

## 1. Source inference and lifting

Connect the source judgments to one checked Core. Track lexical bindings,
the consuming schema, and the refinement evidence alongside the inferred
type, optionality, shape, domain, order, and contextual requirements. Prove
that successful elaboration constructs a well-typed Core term, assigns a
unique checked meaning, and records the requirements its execution uses.

Start with ordinary functions and the table helpers that make Rupam distinctive.
Infer parameter roles from their uses, carry reduction and transform scope
separately for each parameter, and preserve latent requirements when a
function is passed as a value. Instantiate Hindley–Milner schemes with fresh
type, shape, row, and policy variables. Recheck deferred fields in each
consuming schema while retaining resolved lexical bindings. Prove that
common-type selection, optional lifting, and literal defaulting insert only
the coercions permitted by the spec. C41–C42, C59–C63, C100–C104, and C124
exercise the interactions. [Types](../spec/types.md)
· [Toohey et al.'s Lean development](https://github.com/mtoohey31/tabular-types)
· [OutsideIn(X)](https://simon.peytonjones.org/assets/pdfs/outsideinx.pdf)

The spec currently gives two accounts of `Column<T>`. The inference-core
section permits it on parameters, while the signature section restricts it
to display. The checked calculus uses internal role tags. A parser theorem
needs an agreed rule for the surface spelling. [Types](../spec/types.md)

## 2. Guarded evaluation and helper extraction

Give the checked `Guarded` construct one semantics for conditionals,
matches, fallback, and Boolean short-circuiting. Keep the observation domain,
enclosing group, and active mask explicit. Prove that nested guards intersect
their masks, an inactive operand produces no failure, and a demanded
reduction evaluates over the whole group. A refinement derived from a guard
applies only to the rows that passed it.

State extraction equivalence for a hygienic, well-typed helper whose
contextual arguments retain the same consumption sites. Compare successful
values, `None`, row identity, order, and failures. Ordinary arguments
evaluate eagerly, while deferred expressions are rechecked at the consuming
stage. Moving a guarded computation into an eager argument therefore needs
its own proof. Work through C110, C119, C123, and C124 before extending the
theorem to more syntax. [Operational semantics](semantics.md)
· [Cheney, Lindley and Wadler](https://www.research.ed.ac.uk/files/18383912/Cheney_Lindley_ET_AL_2013_A_practical_theory_of_language_integrated_query.pdf)

## 3. Refinements and locked evidence

Prove the inference rules for presence, exact integer bounds, floating-point
rounding, and reduction definedness. Index snapshot evidence by input
identity, stage, target assumptions, group, and active rows. Require an
explicit preservation theorem for transport across stages. Prove that
generalisation cannot capture snapshot facts and that removing a check
preserves all outcomes the locked environment permits. Use C114, C117, C120,
and C122 to exercise rejected evidence. [Refinements](refinements.md)
· [Locking](../spec/session.md#evidence-and-locking)

## 4. Table verbs and the standard library

Finish the relational Core with typed schemas, nominal keys, group
membership, and order witnesses. Prove row preservation for mutation,
cardinality and missing-key behavior for joins, and the schema and order
rules for projection, reshaping, and windows. Extend value semantics to
records, variants, patterns, dates, strings, vectors, and every standard
primitive. Check the boundary cases for numerical operations against their
portable contracts. [Tables](../spec/tables.md)
· [Standard library](../spec/stdlib.md)

## 5. Effects, boundaries, and preservation

Model contracts, ingestion, session revisions, and locked execution as
transitions over immutable inputs and structured outcomes. Prove failure
atomicity for committed session state and trace preservation for external
effects. State the Arrow, file-format, and native-library assumptions used
by each backend proof. Runtime performance claims need measurements under
the conditions specified by the project. [Contracts](../spec/contracts.md)
· [Session](../spec/session.md) · [Runtime](../spec/runtime.md)

Maintain a mapping from each normative rule to its Lean definition, theorem,
and conformance fixtures. The full language claim requires checked coverage
of that mapping. Differential tests compare the implementations on concrete
cases. Compiler preservation requires an additional proof or a sound
validator at each translation boundary covered by the claim.
[Conformance](../spec/conformance.md) · [Compiler preservation](metatheory.md#from-language-soundness-to-compiled-correctness)
