import Rupam.MatchSelection

namespace Rupam

private def evaluateEach : (count : Nat) → (value : Fin count → Type) →
    ((index : Fin count) → Outcome (value index)) → Outcome ((index : Fin count) → value index)
  | 0, _, _ => .ok (fun index => Fin.elim0 index)
  | count + 1, value, evaluate => do
    let first ← evaluate 0
    let rest ← evaluateEach count (fun i => value i.succ) (fun i => evaluate i.succ)
    pure (Fin.cases first rest)

private theorem evaluateEach_at (count : Nat) (value : Fin count → Type)
    (evaluate : (index : Fin count) → Outcome (value index)) {evaluated : (index : Fin count) → value index}
    (accepted : evaluateEach count value evaluate = .ok evaluated) (index : Fin count) :
    evaluate index = .ok (evaluated index) := by
  induction count with
  | zero => exact Fin.elim0 index
  | succ count inductionStep =>
    unfold evaluateEach at accepted
    cases firstResult : evaluate 0 with
    | error failure =>
      rw [firstResult] at accepted
      cases accepted
    | ok first =>
      rw [firstResult] at accepted
      simp only [bind, Except.bind] at accepted
      cases restResult : evaluateEach count (fun i => value i.succ) (fun i => evaluate i.succ) with
      | error failure =>
        rw [restResult] at accepted
        cases accepted
      | ok rest =>
        rw [restResult] at accepted
        have same : Fin.cases first rest = evaluated := Except.ok.inj accepted
        rw [← same]
        refine Fin.cases ?firstCase ?laterCase index
        · exact firstResult
        · intro position
          exact inductionStep _ _ restResult position

variable {scope : EvaluationScope} {rows : List scope.Member}

def ValuesAt.selectedArm (choices : ValuesAt rows (Fin count)) (arm : Fin count) : List scope.Member :=
  choices.select (fun choice => decide (choice = arm))

structure MatchExecution (choices : ValuesAt rows (Fin count)) (result : Type) where
  atArm : (arm : Fin count) → ValuesAt (choices.selectedArm arm) result

def MatchExecution.values {choices : ValuesAt rows (Fin count)}
    (execution : MatchExecution choices result) : ValuesAt rows result :=
  fun row member =>
    let arm := choices row member
    execution.atArm arm row (choices.mem_select (fun choice => decide (choice = arm)) member
      (decide_eq_true rfl))

def ScopedCalculation.runArms (choices : ValuesAt rows (Fin count))
    (arms : Fin count → ScopedCalculation scope result) : Outcome (MatchExecution choices result) :=
  MatchExecution.mk <$> evaluateEach count (fun arm => ValuesAt (choices.selectedArm arm) result)
    (fun arm => (arms arm).run (choices.selectedArm arm))

theorem ScopedCalculation.runArms_observes_selected_scope (choices : ValuesAt rows (Fin count))
    (arms : Fin count → ScopedCalculation scope result) (arm : Fin count)
    {execution : MatchExecution choices result} (ran : ScopedCalculation.runArms choices arms = .ok execution) :
    (arms arm).run (choices.selectedArm arm) = .ok (execution.atArm arm) := by
  unfold ScopedCalculation.runArms at ran
  cases evaluated : evaluateEach count (fun i => ValuesAt (choices.selectedArm i) result)
      (fun i => (arms i).run (choices.selectedArm i)) with
  | error failure =>
    rw [evaluated] at ran
    cases ran
  | ok values =>
    rw [evaluated] at ran
    cases ran
    exact evaluateEach_at count _ _ evaluated arm

theorem ScopedCalculation.unselected_match_arm_cannot_fail (choices : ValuesAt rows (Fin count))
    (arms : Fin count → ScopedCalculation scope result) (arm : Fin count)
    (inactive : ∀ row member, choices row member ≠ arm) :
    ∃ values, (arms arm).run (choices.selectedArm arm) = .ok values := by
  apply (arms arm).branch_inactive
  intro row member
  simp [inactive row member]

def ScopedCalculation.matchArms (subject : ScopedCalculation scope α)
    (tests : List (α → Outcome Bool)) (covers : ∀ value, selectArm value tests ≠ .ok none)
    (arms : Fin tests.length → ScopedCalculation scope result) : ScopedCalculation scope result :=
  ⟨fun first rest => do
    let rows := first :: rest
    let subjects ← subject.run rows
    let choices ← ValuesAt.collect rows fun row member =>
      selectExhaustiveArm (subjects row member) tests (covers (subjects row member))
    let execution ← ScopedCalculation.runArms choices arms
    pure execution.values⟩

theorem ScopedCalculation.matchArms_demand (subject : ScopedCalculation scope α)
    (tests : List (α → Outcome Bool)) (covers : ∀ value, selectArm value tests ≠ .ok none)
    (arms : Fin tests.length → ScopedCalculation scope result)
    (first : scope.Member) (rest : List scope.Member) {subjects : ValuesAt (first :: rest) α}
    (observed : subject.run (first :: rest) = .ok subjects) :
    (subject.matchArms tests covers arms).run (first :: rest) = (do
      let choices ← ValuesAt.collect (first :: rest) (fun row member =>
        selectExhaustiveArm (subjects row member) tests (covers (subjects row member)))
      let execution ← ScopedCalculation.runArms choices arms
      pure execution.values) := by
  change (do
    let values ← subject.run (first :: rest)
    let choices ← ValuesAt.collect (first :: rest) (fun row member =>
      selectExhaustiveArm (values row member) tests (covers (values row member)))
    let execution ← ScopedCalculation.runArms choices arms
    pure execution.values) = _
  rw [observed]
  rfl

end Rupam
