import Rupam.Deferred

namespace Rupam

structure FieldRequirement where
  name : String
  type : Ty

def FieldRequirement.Holds (field : FieldRequirement) (stage : Stage) : Prop :=
  stage.schema field.name = some field.type

def DeferredReference.fields : DeferredReference captured t → List FieldRequirement
  | .lexical _ => []
  | .column name => [⟨name, t⟩]

theorem DeferredReference.fields_iff (reference : DeferredReference captured t) (stage : Stage) :
    (∀ field ∈ reference.fields, field.Holds stage) ↔ reference.Available stage := by
  cases reference <;> simp [fields, FieldRequirement.Holds, Available]

def DeferredInputs.fields : DeferredInputs captured parameters → List FieldRequirement
  | .nil => []
  | .cons head tail => head.fields ++ tail.fields

theorem DeferredInputs.fields_iff (inputs : DeferredInputs captured parameters) (stage : Stage) :
    (∀ field ∈ inputs.fields, field.Holds stage) ↔
      ∀ {t} (param : Parameter parameters t), (inputs.reference param).Available stage := by
  induction inputs with
  | nil =>
    constructor
    · intro _ _ param
      nomatch param
    · intro _ _ member
      exact False.elim (List.not_mem_nil member)
  | cons head tail tailAgrees =>
    constructor
    · intro allFields t param
      cases param with
      | first =>
        apply (head.fields_iff stage).mp
        intro field member
        exact allFields field (List.mem_append_left _ member)
      | next param =>
        apply tailAgrees.mp _ param
        intro field member
        exact allFields field (List.mem_append_right _ member)
    · intro allParameters field member
      cases List.mem_append.mp member with
      | inl neededHere =>
        exact (head.fields_iff stage).mpr (allParameters .first) field neededHere
      | inr neededLater =>
        exact (tailAgrees.mpr (fun param => allParameters (.next param))) field neededLater

theorem DeferredInputs.binding_preserved_by_schema_agreement (inputs : DeferredInputs captured parameters)
    (lexical : ArgumentValues captured) (first second : Stage)
    (agree : ∀ field ∈ inputs.fields, first.schema field.name = second.schema field.name) :
    (∃ bound, inputs.bind first lexical = .ok bound) ↔
      (∃ bound, inputs.bind second lexical = .ok bound) := by
  rw [inputs.bind_succeeds_iff first lexical, inputs.bind_succeeds_iff second lexical,
    ← inputs.fields_iff first, ← inputs.fields_iff second]
  constructor
  · intro checked field member
    have present := checked field member
    unfold FieldRequirement.Holds at present ⊢
    rw [← agree field member]
    exact present
  · intro checked field member
    have present := checked field member
    unfold FieldRequirement.Holds at present ⊢
    rw [agree field member]
    exact present

end Rupam
