# The core calculus λR

λR is the fragment of TransformIR interpreted by the reference evaluator.

## Judgment

    Γ; Δ; Φ ⊢ e : τ^{s,d,o} | C

Under bindings Γ, analytical context Δ and evidence Φ, expression e has type τ
with shape s, observation domain d and order o, leaving requirements C for the
context that consumes e.

## Initial rules

Aligned column arithmetic:

    x : Column_d(A)   y : Column_d(A)   Number(A)
    ─────────────────────────────────────────────
               x + y : Column_d(A)

Scalar broadcasting:

    x : Column_d(A)   y : A   Number(A)
    ───────────────────────────────────
           x + y : Column_d(A)

Optional lifting: if f : A → B then f : A? → B?, propagating absence and
evaluating f only on present arguments.

Grouped reduction:

    x : Column_d(A)   g : d → K
    ───────────────────────────────
    mean_g(x) : Family_K(Float?)

A family is reindexed over d through g; this is how grouped `mutate` aligns a
reduction with its observations.

Each rule added here gets a conformance fixture before the backends
implement it.
