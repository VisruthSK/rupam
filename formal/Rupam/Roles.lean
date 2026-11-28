import Rupam.Elaboration

namespace Rupam

inductive ParameterRole where
  | value
  | column
  | key
  | selector
  deriving DecidableEq

inductive ParameterUse where
  | value
  | column
  | key
  | selector
  deriving DecidableEq

inductive RoleAccepts : ParameterRole → ParameterUse → Prop where
  | value : RoleAccepts .value .value
  | column : RoleAccepts .column .column
  | key : RoleAccepts .key .key
  | keyAsColumn : RoleAccepts .key .column
  | selector : RoleAccepts .selector .selector

def ParameterRole.accepts : ParameterRole → ParameterUse → Bool
  | .value, .value | .column, .column | .key, .key | .key, .column | .selector, .selector => true
  | _, _ => false

theorem ParameterRole.accepts_iff (role : ParameterRole) (usage : ParameterUse) :
    role.accepts usage = true ↔ RoleAccepts role usage := by
  constructor
  · intro accepted
    cases role <;> cases usage <;> first | constructor | cases accepted
  · intro accepted
    cases accepted <;> rfl

inductive RoleError where
  | incompatibleUses
  | annotationRequired

structure CheckedRole (uses : List ParameterUse) where
  role : ParameterRole
  acceptsEveryUse : ∀ usage ∈ uses, RoleAccepts role usage

def ParameterRole.check (role : ParameterRole) (uses : List ParameterUse) :
    Except RoleError (CheckedRole uses) :=
  if passed : uses.all role.accepts = true then
    .ok ⟨role, fun usage member =>
      (role.accepts_iff usage).mp (List.all_eq_true.mp passed usage member)⟩
  else .error .incompatibleUses

theorem ParameterRole.check_succeeds_iff (role : ParameterRole) (uses : List ParameterUse) :
    (∃ checked, role.check uses = .ok checked) ↔ ∀ usage ∈ uses, RoleAccepts role usage := by
  constructor
  · intro ⟨checked, result⟩
    unfold check at result
    split at result
    · rename_i passed
      exact fun usage member => (role.accepts_iff usage).mp (List.all_eq_true.mp passed usage member)
    · cases result
  · intro acceptsEveryUse
    have passed : uses.all role.accepts = true := List.all_eq_true.mpr
      (fun usage member => (role.accepts_iff usage).mpr (acceptsEveryUse usage member))
    exact ⟨⟨role, acceptsEveryUse⟩, by simp [check, passed]⟩

def ParameterRole.infer (uses : List ParameterUse) : Except RoleError (CheckedRole uses) :=
  if uses.contains .key then ParameterRole.key.check uses
  else if uses.contains .column then ParameterRole.column.check uses
  else if uses.contains .selector then ParameterRole.selector.check uses
  else if uses.contains .value then ParameterRole.value.check uses
  else .error .annotationRequired

theorem ParameterRole.key_and_column :
    (infer [.key, .column]).map CheckedRole.role = .ok .key := rfl

theorem ParameterRole.value_and_column_incompatible :
    infer [.value, .column] = .error .incompatibleUses := rfl

-- A key parameter has one name. Its value reads use that same name at the
-- consuming stage, through the ordinary deferred-column elaborator.
structure KeyArgument (t : Ty) where
  name : String

def KeyArgument.asColumn (key : KeyArgument t) : DeferredColumn t where
  captured := []
  lexical := fun {_} parameter => nomatch parameter
  parameters := [t]
  inputs := .cons (.column key.name) .nil
  body := .arg .first

theorem KeyArgument.asColumn_reads_key (key : KeyArgument t) (stage : Stage)
    (present : stage.schema key.name = some t) :
    key.asColumn.lower stage = .ok (.column (stage.read key.name t present)) := by
  simp [KeyArgument.asColumn, DeferredColumn.lower, DeferredInputs.bind, DeferredReference.bind, present,
    bind, Except.bind, pure, Except.pure, ScalarBody.lower, BoundInputs.argument,
    LiftArgument.toCore]

end Rupam
