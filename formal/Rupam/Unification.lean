import Rupam.Schemes

namespace Rupam

abbrev TypeAssignment := Nat → Monotype

structure TypeEquation where
  left : Monotype
  right : Monotype

abbrev TypeEquation.Holds (assignment : TypeAssignment) (equation : TypeEquation) : Prop :=
  equation.left.substitute assignment = equation.right.substitute assignment

structure SolvedEquations (equations : List TypeEquation) where
  assignment : TypeAssignment
  sound : ∀ equation ∈ equations, equation.Holds assignment

inductive UnificationError where
  | infiniteType (name : Nat)
  | incompatible (left right : Monotype)
  | unsolved
  | stepLimit

private def TypeAssignment.bind (previous : TypeAssignment) (name : Nat) (value : Monotype) :
    TypeAssignment :=
  fun input => (previous input).substitute (fun identifier =>
    if identifier = name then value else .variable identifier)

private def simplifyEquations :
    Nat → List TypeEquation → TypeAssignment → Except UnificationError TypeAssignment
  | _, [], assignment => .ok assignment
  | 0, _ :: _, _ => .error .stepLimit
  | fuel + 1, equation :: rest, assignment =>
    let left := equation.left.substitute assignment
    let right := equation.right.substitute assignment
    if left = right then simplifyEquations fuel rest assignment
    else
      match left, right with
      | .variable name, value | value, .variable name =>
        if value.variables.contains name then .error (.infiniteType name)
        else simplifyEquations fuel rest (assignment.bind name value)
      | .arrow a b, .arrow c d =>
        simplifyEquations fuel (⟨a, c⟩ :: ⟨b, d⟩ :: rest) assignment
      | _, _ => .error (.incompatible left right)

def checkTypeAssignment (equations : List TypeEquation) (assignment : TypeAssignment) :
    Except UnificationError (SolvedEquations equations) :=
  if accepted : equations.all (fun equation => decide (equation.Holds assignment)) = true then
    .ok ⟨assignment, fun equation member =>
      of_decide_eq_true (List.all_eq_true.mp accepted equation member)⟩
  else .error .unsolved

theorem checkTypeAssignment_succeeds_iff (equations : List TypeEquation)
    (assignment : TypeAssignment) :
    (∃ result, checkTypeAssignment equations assignment = .ok result) ↔
      ∀ equation ∈ equations, equation.Holds assignment := by
  constructor
  · intro ⟨result, checked⟩
    unfold checkTypeAssignment at checked
    split at checked
    · rename_i accepted
      exact fun equation member => of_decide_eq_true (List.all_eq_true.mp accepted equation member)
    · cases checked
  · intro valid
    have accepted : equations.all (fun equation => decide (equation.Holds assignment)) = true :=
      List.all_eq_true.mpr (fun equation member => decide_eq_true (valid equation member))
    exact ⟨⟨assignment, valid⟩, by simp [checkTypeAssignment, accepted]⟩

-- The search can fail or run out of steps. Every accepted substitution is
-- checked against all original equations, including ones simplified away.
def solveTypeEquations (fuel : Nat) (equations : List TypeEquation) :
    Except UnificationError (SolvedEquations equations) := do
  let candidate ← simplifyEquations fuel equations Monotype.variable
  checkTypeAssignment equations candidate

end Rupam
