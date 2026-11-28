import Rupam.HigherOrder

namespace Rupam

def Local.index : Local locals t → Nat
  | .first => 0
  | .next binding => binding.index + 1

-- Uses are static. Both arms contribute dependencies even though execution
-- visits only the positions selected by their guards.
def ContextTerm.uses : ContextTerm payload locals t requirements → Nat → Bool
  | .variable binding, slot => binding.index == slot
  | .scalar _, _ | .vector _, _ | .pointwise _, _
  | .reducer .., _ | .vectorReducer .., _ | .orderedReducer .., _ => false
  | .mapResult _ input, slot => input.uses slot
  | .combine _ left right, slot => left.uses slot || right.uses slot
  | .conditional _ condition yes no, slot => condition.uses slot || yes.uses slot || no.uses slot
  | .lambda body, slot => body.uses (slot + 1)
  | .apply fn argument, slot => fn.uses slot || argument.uses slot

theorem ContextTerm.eval_agrees_on_used (term : ContextTerm payload locals t requirements)
    (context : ConsumptionContext) (first second : ContextEnvironment payload context locals)
    (supplies : Supplies context requirements)
    (agree : ∀ {type} (binding : Local locals type),
      term.uses binding.index = true → first binding = second binding) :
    term.eval context first supplies = term.eval context second supplies := by
  induction term with
  | «variable» binding =>
    exact agree binding (by simp [uses])
  | scalar value => rfl
  | vector values => rfl
  | pointwise operation => rfl
  | reducer kind policy operation => rfl
  | vectorReducer policy operation => rfl
  | orderedReducer kind policy operation => rfl
  | mapResult operation input inputAgrees =>
    exact congrArg (fun calculation => calculation.map operation)
      (inputAgrees first second supplies (fun binding used => agree binding used))
  | combine operation left right leftAgrees rightAgrees =>
    simp only [ContextTerm.eval]
    congr 1
    · apply leftAgrees
      intro type binding used
      exact agree binding (by simp [uses, used])
    · apply rightAgrees
      intro type binding used
      exact agree binding (by simp [uses, used])
  | conditional test condition yes no conditionAgrees yesAgrees noAgrees =>
    simp only [ContextTerm.eval]
    congr 1
    · congr 1
      apply conditionAgrees
      intro type binding used
      exact agree binding (by simp [uses, used])
    · apply yesAgrees
      intro type binding used
      exact agree binding (by simp [uses, used])
    · apply noAgrees
      intro type binding used
      exact agree binding (by simp [uses, used])
  | lambda body bodyAgrees =>
    funext forCall argument
    apply bodyAgrees
    intro type binding used
    cases binding with
    | first => rfl
    | next enclosing => exact agree enclosing used
  | @apply _ _ _ _ functionNeeds argumentNeeds fn argument fnAgrees argAgrees =>
    have sameFunction := fnAgrees first second (supplies.split functionNeeds).1
      (fun binding used => agree binding (by simp [uses, used]))
    have sameArgument := argAgrees first second
      ((supplies.split functionNeeds).2.split argumentNeeds).1
      (fun binding used => agree binding (by simp [uses, used]))
    simp only [ContextTerm.eval, sameFunction, sameArgument]

end Rupam
