# Operational semantics

Big-step evaluation of λR over values, columns and tables, with failure as an
explicit outcome: overflow, nonfinite results, failed obligations and policy
`error`. Guarded constructs (`if`, `match`, `??`, `and`, `or`) evaluate only
selected branches; a selected reduction reads its whole group. The OCaml
reference evaluator is the executable form of these rules, and the Lean model
is checked against it on shared fixtures.
