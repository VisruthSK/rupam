import Rupam.OrdinaryInference

namespace Rupam

def OrdinaryCore.substituteTypes (replace : TypeAssignment) :
    OrdinaryCore context type →
      OrdinaryCore (context.map (Monotype.substitute replace)) (type.substitute replace)
  | .literal t value => .literal t value
  | .reference slot found => .reference slot (by simp [List.getElem?_map, found])
  | .lambda body => .lambda (body.substituteTypes replace)
  | .apply fn argument => .apply (fn.substituteTypes replace) (argument.substituteTypes replace)

theorem OrdinaryCore.erase_substituteTypes (term : OrdinaryCore context type)
    (replace : TypeAssignment) :
    (term.substituteTypes replace).erase = term.erase := by
  induction term with
  | literal _ _ => rfl
  | reference _ _ => rfl
  | lambda _ bodyErases => simp only [substituteTypes, erase, bodyErases]
  | apply fn argument fnErases argumentErases =>
    change OrdinarySource.apply (fn.substituteTypes replace).erase
      (argument.substituteTypes replace).erase = OrdinarySource.apply fn.erase argument.erase
    rw [fnErases, argumentErases]

def OrdinaryCore.recontext (same : source = target) (term : OrdinaryCore source type) :
    OrdinaryCore target type :=
  same ▸ term

theorem OrdinaryCore.erase_recontext (same : source = target) (term : OrdinaryCore source type) :
    (term.recontext same).erase = term.erase := by
  cases same
  rfl

def OrdinaryCore.specialiseDefinition (term : OrdinaryCore context type)
    (instanceFor : Nat → Monotype) :
    OrdinaryCore context ((Scheme.generalise (context.map Scheme.mono) type []).instantiate instanceFor) :=
  let scheme := Scheme.generalise (context.map Scheme.mono) type []
  (term.substituteTypes (scheme.replacement instanceFor)).recontext
    (Scheme.generalise_preserves_context context type [] instanceFor)

theorem OrdinaryCore.specialiseDefinition_erases (term : OrdinaryCore context type)
    (instanceFor : Nat → Monotype) :
    (term.specialiseDefinition instanceFor).erase = term.erase := by
  let scheme := Scheme.generalise (context.map Scheme.mono) type []
  have same : context.map (Monotype.substitute (scheme.replacement instanceFor)) = context :=
    Scheme.generalise_preserves_context context type [] instanceFor
  change ((term.substituteTypes (scheme.replacement instanceFor)).recontext same).erase = term.erase
  exact (erase_recontext same _).trans (erase_substituteTypes term _)

structure CheckedInstance (definition : OrdinaryCore context type) (expected : Monotype) where
  core : OrdinaryCore context expected
  sameSource : core.erase = definition.erase

-- Instance search may propose changes to lexical metavariables. Recheck the
-- requested type after substituting only the scheme's quantified variables.
def OrdinaryCore.checkInstance (definition : OrdinaryCore context type)
    (expected : Monotype) (fuel : Nat) : Except UnificationError (CheckedInstance definition expected) := do
  let scheme := Scheme.generalise (context.map Scheme.mono) type []
  let reserved := context.flatMap Monotype.variables ++ type.variables ++ expected.variables
  let fresh := firstUnusedTypeName reserved
  let solved ← solveTypeEquations fuel [⟨scheme.freshInstance fresh, expected⟩]
  let instanceFor := fun name => solved.assignment (fresh + name)
  if matchesType : scheme.instantiate instanceFor = expected then
    let specialized := definition.specialiseDefinition instanceFor
    .ok ⟨specialized.retype matchesType, (erase_retype matchesType specialized).trans
      (definition.specialiseDefinition_erases instanceFor)⟩
  else
    .error .unsolved

end Rupam
