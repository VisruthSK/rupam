import Rupam.Scope

namespace Rupam

-- The session fixes these identities before recording evidence. A scope and
-- its active observations are part of the key even when the stage is shared.
structure EvidenceSite where
  snapshot : Nat
  stage : Nat
  target : Nat
  scope : EvaluationScope
  active : List scope.Member

structure CheckEvidence (_site : EvidenceSite)
    (permitted : Outcome α → Prop) (test : α → Bool) : Prop where
  proves : ∀ value, permitted (.ok value) → test value = true

def guardedCheck (test : α → Bool) (onFailure : Failure) (evaluated : Outcome α) : Outcome α := do
  let value ← evaluated
  if test value then .ok value else .error onFailure

theorem CheckEvidence.eliminate {α : Type} {permitted : Outcome α → Prop} {test : α → Bool}
    (evidence : CheckEvidence site permitted test)
    (result : Outcome α) (allowed : permitted result) (onFailure : Failure) :
    guardedCheck test onFailure result = result := by
  cases result with
  | error failure => rfl
  | ok value =>
    have passes := evidence.proves value allowed
    change (if test value then Except.ok value else Except.error onFailure) =
      (Except.ok value : Outcome α)
    rw [passes] <;> rfl

theorem CheckEvidence.counterexample (site : EvidenceSite) (permitted : Outcome α → Prop)
    (test : α → Bool) (value : α) (allowed : permitted (.ok value)) (fails : test value = false) :
    ¬ CheckEvidence site permitted test := by
  intro evidence
  have contradiction := evidence.proves value allowed
  rw [fails] at contradiction
  cases contradiction

-- Moving evidence requires an explicit semantic preservation fact. Identity
-- alone is insufficient when the computation or its permitted outcomes change.
theorem CheckEvidence.transport {α : Type} {permitted : Outcome α → Prop} {test : α → Bool}
    (old : CheckEvidence oldSite permitted test)
    (newSite : EvidenceSite) (newPermitted : Outcome α → Prop)
    (preserves : ∀ value, newPermitted (.ok value) → permitted (.ok value)) :
    CheckEvidence newSite newPermitted test :=
  ⟨fun value allowed => old.proves value (preserves value allowed)⟩

theorem EvidenceSite.changed_stage (first second : EvidenceSite)
    (changed : first.stage ≠ second.stage) : first ≠ second := by
  intro same
  exact changed (congrArg EvidenceSite.stage same)

end Rupam
