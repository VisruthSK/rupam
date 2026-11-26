# Coeffects

Contextual requirements (missing policies, grouping, observation order) are
coeffects: demands an expression places on its context, recorded in C and
discharged at consumption sites. A helper's scheme carries its unresolved
requirements; an application discharges them from an explicit argument, the
enclosing verb or the lexical `set` default, in that order. Requirements
compose by union and propagate through higher-order calls. Requirements fixed
inside a helper are never overridden.

The implicit-parameter example in the coeffects papers separates the
requirements available at a function's definition from those left for the
caller. The 2014 calculus also distinguishes whole-context requirements from
annotations on individual variables. In Rupam, a missing policy is supplied
by the analytical context, and each decision point can fix its policy
lexically. The spec determines which policies stay fixed and the priority
of the remaining sources. [ICALP 2013, §§1–2](https://tomasp.net/academic/papers/coeffects/coeffects-icalp.pdf)
· [ICFP 2014, §§2.4, 3.5, 4.2](https://www.doc.ic.ac.uk/~dorchard/publ/coeffects-icfp14.pdf)
· [Implicit Parameters, §§2–3](https://launchbury.blog/wp-content/uploads/2019/01/implicit-paramters.pdf)
· [Missing values, Scope](../spec/missing.md#scope)

The Lean judgment `DischargesPolicy context demand policy` describes the
four ways to resolve one decision point. Its soundness and completeness
proofs connect the judgment to the executable resolver. A separate proof
shows that filling a demand from two contexts in order is equivalent to
filling it from their first available policy. Grouping and reductions
resolve independently, so a group-specific override leaves reduction policy
selection unchanged. [Lean policy model](../../formal/Rupam/Policy.lean)

Combining the requirements of subexpressions and prioritising policy
suppliers are two operations. The policy resolution proofs cover the latter
after a decision point has been identified. Abstraction, application and
generalisation still need a typing judgment that tracks latent requirements
through function values. An extraction proof will need to account for that
judgment and for lexical binding identity, stage-relative column resolution
and guarded evaluation. [Types, Contextual requirements](../spec/types.md#contextual-requirements)
