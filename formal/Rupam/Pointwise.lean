import Rupam.Scope

namespace Rupam

variable {scope : EvaluationScope}

-- A pure scalar operation may use any payload and return a checked failure.
-- A product payload represents several already aligned arguments.
def ScopedCalculation.map (input : ScopedCalculation scope α) (operation : α → Outcome β) :
    ScopedCalculation scope β :=
  ⟨fun first rest => do
    let positions := first :: rest
    let arguments ← input.run positions
    ValuesAt.collect positions fun position member => operation (arguments position member)⟩

theorem ScopedCalculation.map_at (input : ScopedCalculation scope α)
    (operation : α → Outcome β) (positions : List scope.Member)
    {arguments : ValuesAt positions α} {output : ValuesAt positions β}
    (argumentsRead : input.run positions = .ok arguments)
    (lifted : (input.map operation).run positions = .ok output)
    (position : scope.Member) (member : position ∈ positions) :
    operation (arguments position member) = .ok (output position member) := by
  cases positions with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first rest =>
    change (do
      let values ← input.run (first :: rest)
      ValuesAt.collect (first :: rest) fun p h => operation (values p h)) = .ok output at lifted
    rw [argumentsRead] at lifted
    exact ValuesAt.collect_at lifted position member

theorem ScopedCalculation.map_succeeds_iff (input : ScopedCalculation scope α)
    (operation : α → Outcome β) (positions : List scope.Member)
    {arguments : ValuesAt positions α}
    (argumentsRead : input.run positions = .ok arguments) :
    (∃ output, (input.map operation).run positions = .ok output) ↔
      ∀ position member, ∃ value, operation (arguments position member) = .ok value := by
  constructor
  · intro ⟨output, lifted⟩ position member
    exact ⟨output position member, input.map_at operation positions argumentsRead lifted position member⟩
  · intro each
    cases positions with
    | nil => exact ⟨ValuesAt.empty, rfl⟩
    | cons first rest =>
      obtain ⟨result, collected⟩ := ValuesAt.collect_succeeds (first :: rest)
        (fun position member => operation (arguments position member)) each
      refine ⟨result, ?_⟩
      change (do
        let values ← input.run (first :: rest)
        ValuesAt.collect (first :: rest) fun p h => operation (values p h)) = .ok result
      rw [argumentsRead]
      exact collected

end Rupam
