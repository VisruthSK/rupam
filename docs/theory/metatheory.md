# Metatheory

| Property | Statement | Status |
| --- | --- | --- |
| Type preservation | evaluation preserves types, shapes and domains | conjecture |
| Domain safety | incompatible observations cannot combine implicitly | conjecture |
| Context safety | requirements are discharged at consumption sites | conjecture |
| Extraction equivalence | extracting an expression into a helper preserves meaning | conjecture |
| Grouping stability | mutation cannot invalidate an established grouping witness | conjecture |
| Guard safety | inactive computations cannot fail | conjecture |
| Refinement soundness | inferred predicates hold for evaluated values | conjecture |
| Evidence locality | snapshot evidence cannot escape its scope | conjecture |
| Backend preservation | compiled programs preserve the specified observable semantics | tested by conformance |

Extraction equivalence distinguishes Rupam from dataframe languages with
nonstandard evaluation: a reusable helper means what its inline body means.
