import Rupam.Scope

namespace Rupam

variable {α : Type}

private def laterArm (result : Outcome (Option Nat)) : Outcome (Option Nat) :=
  result.map (Option.map Nat.succ)

-- Each test includes its pattern and guard. A later test runs only when
-- the current test succeeds with false.
def selectArm (subject : α) : List (α → Outcome Bool) → Outcome (Option Nat)
  | [] => .ok none
  | test :: remaining =>
    match test subject with
    | .error failure => .error failure
    | .ok true => .ok (some 0)
    | .ok false => laterArm (selectArm subject remaining)

inductive ChoosesArm (subject : α) : List (α → Outcome Bool) → Outcome (Option Nat) → Prop where
  | exhausted : ChoosesArm subject [] (.ok none)
  | testFailure (failure : Failure) (failed : test subject = .error failure) :
      ChoosesArm subject (test :: remaining) (.error failure)
  | accepted (passed : test subject = .ok true) :
      ChoosesArm subject (test :: remaining) (.ok (some 0))
  | rejected (notHere : test subject = .ok false) (later : ChoosesArm subject remaining result) :
      ChoosesArm subject (test :: remaining) (laterArm result)

theorem ChoosesArm.selects (derivation : ChoosesArm subject arms result) :
    selectArm subject arms = result := by
  induction derivation with
  | exhausted => rfl
  | testFailure _ failed => simp [selectArm, failed]
  | accepted passed => simp [selectArm, passed]
  | rejected notHere later agrees => simp [selectArm, notHere, agrees]

theorem selectArm_satisfies_rules (subject : α) (arms : List (α → Outcome Bool)) :
    ChoosesArm subject arms (selectArm subject arms) := by
  induction arms with
  | nil => exact .exhausted
  | cons test remaining later =>
    unfold selectArm
    cases tested : test subject with
    | error failure => exact .testFailure failure tested
    | ok result =>
      cases result with
      | true => exact .accepted tested
      | false => exact .rejected tested later

theorem ChoosesArm.index_in_range (derivation : ChoosesArm subject arms result)
    (selected : result = .ok (some index)) : index < arms.length := by
  induction derivation generalizing index with
  | exhausted => cases selected
  | testFailure _ _ => cases selected
  | accepted _ =>
    have first : 0 = index := Option.some.inj (Except.ok.inj selected)
    subst index
    exact Nat.zero_lt_succ _
  | @rejected _ remaining outcome _ prior inductionStep =>
    cases outcome with
    | error failure => cases selected
    | ok value =>
      cases value with
      | none => cases selected
      | some earlier =>
        have next : earlier + 1 = index := Option.some.inj (Except.ok.inj selected)
        subst index
        exact Nat.succ_lt_succ (inductionStep rfl)

def selectExhaustiveArm (subject : α) (arms : List (α → Outcome Bool))
    (covers : selectArm subject arms ≠ .ok none) : Outcome (Fin arms.length) :=
  match selected : selectArm subject arms with
  | .error failure => .error failure
  | .ok none => False.elim (covers selected)
  | .ok (some index) => .ok ⟨index, (selectArm_satisfies_rules subject arms).index_in_range selected⟩

theorem selectArm_ignores_later_tests (subject : α)
    (preceding : List (α → Outcome Bool)) (selected : α → Outcome Bool)
    (later : List (α → Outcome Bool))
    (rejected : ∀ test ∈ preceding, test subject = .ok false)
    (accepted : selected subject = .ok true) :
    selectArm subject (preceding ++ selected :: later) = .ok (some preceding.length) := by
  induction preceding with
  | nil => simp [selectArm, accepted]
  | cons test remaining agrees =>
    have notHere := rejected test List.mem_cons_self
    have rest := agrees (fun candidate member => rejected candidate (List.mem_cons_of_mem _ member))
    simp [selectArm, notHere, rest, laterArm]
    rfl

variable {scope : EvaluationScope} {rows : List scope.Member}

def ValuesAt.armRows (choices : ValuesAt rows (Option Nat)) (arm : Nat) : List scope.Member :=
  choices.select (fun choice => decide (choice = some arm))

theorem ValuesAt.armRows_disjoint (choices : ValuesAt rows (Option Nat))
    (row : scope.Member) (left right : Nat)
    (inLeft : row ∈ choices.armRows left) (inRight : row ∈ choices.armRows right) : left = right := by
  obtain ⟨member, leftPass⟩ := (choices.mem_select_iff (fun choice => decide (choice = some left)) row).mp inLeft
  obtain ⟨_, rightPass⟩ := (choices.mem_select_iff (fun choice => decide (choice = some right)) row).mp inRight
  have leftChoice : choices row member = some left := of_decide_eq_true leftPass
  have rightChoice : choices row member = some right := of_decide_eq_true rightPass
  exact Option.some.inj (leftChoice.symm.trans rightChoice)

end Rupam
