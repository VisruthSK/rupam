# Rupam theory

The mathematical account of Rupam, separate from the user-facing
[specification](../spec/README.md), which stays readable without logic. The
calculus is defined here before the implementation grows around it; the Lean 4
mechanisation in `formal/` proves its properties incrementally.

| File | Contents |
| --- | --- |
| [Calculus](calculus.md) | syntax and typing judgments of the core calculus λR |
| [Coeffects](coeffects.md) | contextual requirements and their composition |
| [Domains](domains.md) | observation identity, alignment and grouping |
| [Refinements](refinements.md) | predicates, inference and evidence |
| [Semantics](semantics.md) | operational semantics and failure |
| [Categorical models](categorical.md) | denotational and categorical models |
| [Metatheory](metatheory.md) | theorems, conjectures and proof status |
| [Related work](related-work.md) | comparison with existing research |

Notation: Γ lexical bindings, Δ analytical context (current table, grouping,
policies), Φ refinement evidence, C contextual requirements, τ a value type,
s a shape (scalar or columnar), d an observation domain, o an order witness.
