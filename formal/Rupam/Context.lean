import Rupam.Policy
import Rupam.Scope

namespace Rupam

inductive CollectionKind where
  | vector
  | group
  deriving DecidableEq

def EvaluationScope.kind : EvaluationScope → CollectionKind
  | .vector _ => .vector
  | .group _ => .group

structure OrderWitness (scope : EvaluationScope) where
  rank : scope.Index → Nat
  ordered : scope.positions.Pairwise (fun left right => rank left < rank right)

structure ConsumptionContext where
  scope : EvaluationScope
  policy : PolicyContext := {}
  order : Option (OrderWitness scope) := none

inductive Requirement where
  | collection (kind : CollectionKind)
  | domain (domain : Domain)
  | order
  | missing (policy : PolicyDemand)

def Requirement.Holds (context : ConsumptionContext) : Requirement → Prop
  | .collection kind => context.scope.kind = kind
  | .domain expected => context.scope.shape = .column expected
  | .order => context.order.isSome = true
  | .missing demand => (demand.resolve context.policy).toOption.isSome = true

instance (context : ConsumptionContext) (requirement : Requirement) :
    Decidable (requirement.Holds context) := by
  cases requirement <;> unfold Requirement.Holds <;> infer_instance

abbrev Requirement.Evidence (context : ConsumptionContext) (requirement : Requirement) :=
  PLift (requirement.Holds context)

def Requirement.policyValue (demand : PolicyDemand) (context : ConsumptionContext)
    (evidence : (Requirement.missing demand).Evidence context) : MissingPolicy :=
  (demand.resolve context.policy).toOption.get evidence.down

theorem Requirement.policyValue_matches (demand : PolicyDemand) (context : ConsumptionContext)
    (evidence : (Requirement.missing demand).Evidence context) :
    DischargesPolicy context.policy demand (Requirement.policyValue demand context evidence) := by
  cases resolved : demand.resolve context.policy with
  | error failure =>
    have available := evidence.down
    change (demand.resolve context.policy).toOption.isSome = true at available
    rw [resolved] at available
    cases available
  | ok policy =>
    have chosen : (demand.resolve context.policy).toOption = some policy :=
      congrArg Except.toOption resolved
    have same : Requirement.policyValue demand context evidence = policy :=
      Option.get_of_eq_some evidence.down chosen
    rw [same]
    exact PolicyDemand.resolve_sound resolved

def Requirement.discharge (context : ConsumptionContext) (requirement : Requirement) :
    Except Requirement (requirement.Evidence context) :=
  if valid : requirement.Holds context then .ok ⟨valid⟩
  else .error requirement

theorem Requirement.discharge_complete (context : ConsumptionContext) (requirement : Requirement)
    (evidence : requirement.Evidence context) :
    ∃ checked, requirement.discharge context = .ok checked := by
  exact ⟨evidence, by simp [discharge, evidence.down]⟩

-- Each requirement keeps its own evidence, including its lexically fixed policy.
inductive Supplies (context : ConsumptionContext) : List Requirement → Type where
  | nil : Supplies context []
  | cons : requirement.Evidence context → Supplies context rest →
      Supplies context (requirement :: rest)

def Supplies.split (left : List Requirement) :
    Supplies context (left ++ right) → Supplies context left × Supplies context right
  | evidence => match left, evidence with
    | [], evidence => (.nil, evidence)
    | _ :: tail, .cons proof rest =>
      let (before, after) := rest.split tail
      (.cons proof before, after)

def Supplies.discharge (context : ConsumptionContext) :
    (requirements : List Requirement) → Except Requirement (Supplies context requirements)
  | [] => .ok .nil
  | head :: rest => do
    let evidence ← head.discharge context
    let tail ← Supplies.discharge context rest
    pure (.cons evidence tail)

theorem Supplies.discharge_complete (evidence : Supplies context requirements) :
    ∃ checked, Supplies.discharge context requirements = .ok checked := by
  induction evidence with
  | nil => exact ⟨.nil, rfl⟩
  | cons proof rest tailComplete =>
    obtain ⟨headEvidence, headAccepted⟩ := Requirement.discharge_complete context _ proof
    obtain ⟨tailEvidence, tailAccepted⟩ := tailComplete
    refine ⟨.cons headEvidence tailEvidence, ?_⟩
    rw [discharge, headAccepted]
    change (do
      let tail ← Supplies.discharge context _
      pure (Supplies.cons headEvidence tail)) = _
    rw [tailAccepted]
    rfl

theorem Supplies.discharge_iff (context : ConsumptionContext) (requirements : List Requirement) :
    (∃ checked, Supplies.discharge context requirements = .ok checked) ↔
      Nonempty (Supplies context requirements) := by
  constructor
  · intro ⟨checked, _⟩
    exact ⟨checked⟩
  · intro ⟨evidence⟩
    exact evidence.discharge_complete

end Rupam
