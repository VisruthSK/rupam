import Rupam.StageLinking
import Rupam.ContextUse

namespace Rupam

variable {context : ConsumptionContext}

private def StageEnvironment.tail (environment : StageEnvironment context (head :: rest)) :
    StageEnvironment context rest :=
  ⟨fun {_} binding => environment.lookup (.next binding)⟩

-- A source argument requires a schema only at a stage that consumes it.
def StageInputs.ReadyFor (stage : Stage) :
    StageInputs context locals → (Nat → Bool) → Prop
  | .nil, _ => True
  | .ordinary _ tail, used => tail.ReadyFor stage (fun slot => used (slot + 1))
  | .deferred closure tail, used =>
    (used 0 = true → closure.Ready stage) ∧ tail.ReadyFor stage (fun slot => used (slot + 1))

def StageInputs.forUses :
    StageInputs context locals → (Nat → Bool) → StageEnvironment context locals →
      StageInputs context locals
  | .nil, _, _ => .nil
  | .ordinary value tail, used, unused =>
    .ordinary value (tail.forUses (fun slot => used (slot + 1)) unused.tail)
  | .deferred closure tail, used, unused =>
    let selected := tail.forUses (fun slot => used (slot + 1)) unused.tail
    if used 0 then .deferred closure selected
    else .ordinary (unused.lookup .first) selected

theorem StageInputs.forUses_ready_iff (arguments : StageInputs context locals) (stage : Stage)
    (used : Nat → Bool) (unused : StageEnvironment context locals) :
    (arguments.forUses used unused).Ready stage ↔ arguments.ReadyFor stage used := by
  induction arguments generalizing used with
  | nil => rfl
  | ordinary _ tail available =>
    exact available (fun slot => used (slot + 1)) unused.tail
  | deferred closure tail available =>
    cases kept : used 0 <;> simp [forUses, Ready, ReadyFor, kept, available]

theorem StageInputs.forUses_bindings_agree (arguments : StageInputs context locals)
    (stage : Stage) (aligned : context.scope.shape = .column stage.domain)
    (used : Nat → Bool) (first second : StageEnvironment context locals)
    (firstReady : (arguments.forUses used first).Ready stage)
    (secondReady : (arguments.forUses used second).Ready stage)
    (binding : Local locals type) (live : used binding.index = true) :
    ((arguments.forUses used first).direct stage aligned firstReady).lookup binding =
      ((arguments.forUses used second).direct stage aligned secondReady).lookup binding := by
  induction arguments generalizing used with
  | nil => nomatch binding
  | ordinary value tail inherits =>
    cases binding with
    | first => rfl
    | next binding =>
      exact inherits _ first.tail second.tail firstReady secondReady binding live
  | deferred closure tail inherits =>
    cases kept : used 0 with
    | false =>
      cases binding with
      | first =>
        change used 0 = true at live
        rw [kept] at live
        cases live
      | next binding =>
        simp only [forUses, kept, Bool.false_eq_true, ite_false, Ready] at firstReady secondReady ⊢
        exact inherits _ first.tail second.tail firstReady secondReady binding live
    | true =>
      cases binding with
      | first =>
        simp only [forUses, kept, ite_true]
        rfl
      | next binding =>
        simp only [forUses, kept, ite_true, Ready] at firstReady secondReady ⊢
        exact inherits _ first.tail second.tail firstReady.2 secondReady.2 binding live

theorem ContextElaborates.link_uses_preserves_source (derivation : ContextElaborates locals source result needs)
    (arguments : StageInputs context locals) (stage : Stage)
    (aligned : context.scope.shape = .column stage.domain)
    (first second : StageEnvironment context locals)
    (ready : arguments.ReadyFor stage derivation.core.uses) :
    let used := derivation.core.uses
    let linked := arguments.forUses used first
    let direct := arguments.forUses used second
    let checked := (arguments.forUses_ready_iff stage used second).mpr ready
    derivation.core.linkedRun linked stage aligned =
      .ok (derivation.runSource context (direct.direct stage aligned checked).lookup) := by
  let used := derivation.core.uses
  have firstReady := (arguments.forUses_ready_iff stage used first).mpr ready
  have secondReady := (arguments.forUses_ready_iff stage used second).mpr ready
  change derivation.core.linkedRun (arguments.forUses used first) stage aligned =
    .ok (derivation.runSource context ((arguments.forUses used second).direct stage aligned secondReady).lookup)
  rw [derivation.core.eliminate_deferred_lookup (arguments.forUses used first) stage aligned firstReady,
    ← derivation.run_preserves_source]
  unfold ContextTerm.run
  congr 1
  congr 1
  funext available
  apply congrArg (fun value => (pure value : Except Requirement (result.denote Ty.denote context)))
  apply derivation.core.eval_agrees_on_used
  intro type binding live
  exact arguments.forUses_bindings_agree stage aligned used first second firstReady secondReady binding live

end Rupam
