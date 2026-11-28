import Rupam.Context

namespace Rupam

inductive Monotype where
  | variable (name : Nat)
  | cell (type : Ty)
  | arrow (input output : Monotype)
  deriving DecidableEq

def Monotype.variables : Monotype → List Nat
  | .variable name => [name]
  | .cell _ => []
  | .arrow input output => input.variables ++ output.variables

def firstUnusedTypeName : List Nat → Nat
  | [] => 0
  | name :: remaining => max (name + 1) (firstUnusedTypeName remaining)

theorem firstUnusedTypeName_above (reserved : List Nat) (name : Nat) (member : name ∈ reserved) :
    name < firstUnusedTypeName reserved := by
  induction reserved with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first remaining tailAbove =>
    cases List.mem_cons.mp member with
    | inl same =>
      subst name
      exact Nat.lt_of_lt_of_le (Nat.lt_succ_self first) (Nat.le_max_left _ _)
    | inr inTail =>
      exact Nat.lt_of_lt_of_le (tailAbove inTail) (Nat.le_max_right _ _)

def Monotype.substitute (replace : Nat → Monotype) : Monotype → Monotype
  | .variable name => replace name
  | .cell type => .cell type
  | .arrow input output => .arrow (input.substitute replace) (output.substitute replace)

def Monotype.interpret (assignment : Nat → Type) : Monotype → Type
  | .variable name => assignment name
  | .cell type => type.denote
  | .arrow input output => input.interpret assignment → output.interpret assignment

theorem Monotype.interpret_substitution (type : Monotype) (replace : Nat → Monotype)
    (assignment : Nat → Type) :
    (type.substitute replace).interpret assignment =
      type.interpret (fun name => (replace name).interpret assignment) := by
  induction type with
  | «variable» name => rfl
  | cell type => rfl
  | arrow input output inputAgrees outputAgrees =>
    simp only [substitute, interpret, inputAgrees, outputAgrees]

theorem Monotype.substitute_unchanged (type : Monotype) (replace : Nat → Monotype)
    (unchanged : ∀ name ∈ type.variables, replace name = .variable name) :
    type.substitute replace = type := by
  induction type with
  | «variable» name => exact unchanged name (by simp [variables])
  | cell type => rfl
  | arrow input output inputSame outputSame =>
    have sameInput := inputSame (fun name member => unchanged name (List.mem_append_left _ member))
    have sameOutput := outputSame (fun name member => unchanged name (List.mem_append_right _ member))
    simp only [substitute, sameInput, sameOutput]

structure Scheme where
  body : Monotype
  latent : List Requirement
  quantified : List Nat

def Scheme.mono (type : Monotype) : Scheme := ⟨type, [], []⟩

def Scheme.freeVariables (scheme : Scheme) : List Nat :=
  scheme.body.variables.filter (fun name => !scheme.quantified.contains name)

def Scheme.generalise (lexical : List Scheme) (body : Monotype)
    (latent : List Requirement) : Scheme :=
  { body, latent
    quantified := body.variables.eraseDups.filter
      (fun name => !(lexical.flatMap Scheme.freeVariables).contains name) }

theorem Scheme.quantifies_iff (lexical : List Scheme) (body : Monotype)
    (latent : List Requirement) (name : Nat) :
    name ∈ (generalise lexical body latent).quantified ↔
      name ∈ body.variables ∧ name ∉ lexical.flatMap Scheme.freeVariables := by
  simp [generalise, List.mem_filter]

def Scheme.replacement (scheme : Scheme) (instanceFor : Nat → Monotype) (name : Nat) :
    Monotype :=
  if name ∈ scheme.quantified then instanceFor name else .variable name

def Scheme.instantiate (scheme : Scheme) (instanceFor : Nat → Monotype) : Monotype :=
  scheme.body.substitute (scheme.replacement instanceFor)

theorem Scheme.instantiate_preserves_lexical_name (lexical : List Scheme)
    (body : Monotype) (latent : List Requirement) (instanceFor : Nat → Monotype)
    (name : Nat) (inLexical : name ∈ lexical.flatMap Scheme.freeVariables) :
    (generalise lexical body latent).replacement instanceFor name = .variable name := by
  have free : name ∉ (generalise lexical body latent).quantified :=
    fun quantified => ((quantifies_iff lexical body latent name).mp quantified).2 inLexical
  simp [replacement, free]

theorem Scheme.generalise_preserves_context (context : List Monotype) (body : Monotype)
    (latent : List Requirement) (instanceFor : Nat → Monotype) :
    let scheme := Scheme.generalise (context.map Scheme.mono) body latent
    context.map (Monotype.substitute (scheme.replacement instanceFor)) = context := by
  dsimp
  refine Eq.trans (b := context.map id) ?_ (List.map_id context)
  apply List.map_congr_left
  intro type member
  apply type.substitute_unchanged
  intro name inType
  apply instantiate_preserves_lexical_name
  apply List.mem_flatMap.mpr
  exact ⟨Scheme.mono type, List.mem_map.mpr ⟨type, member, rfl⟩,
    by simpa [mono, freeVariables] using inType⟩

theorem Scheme.instantiate_sound (scheme : Scheme) (instanceFor : Nat → Monotype)
    (assignment : Nat → Type) :
    (scheme.instantiate instanceFor).interpret assignment =
      scheme.body.interpret (fun name => (scheme.replacement instanceFor name).interpret assignment) :=
  scheme.body.interpret_substitution _ _

def Scheme.typeArguments (scheme : Scheme) (lexical arguments : Nat → Type) : Nat → Type :=
  fun name => if name ∈ scheme.quantified then arguments name else lexical name

abbrev Scheme.Value (scheme : Scheme) (lexical : Nat → Type) :=
  (arguments : Nat → Type) → scheme.body.interpret (scheme.typeArguments lexical arguments)

theorem Scheme.instantiate_interpret (scheme : Scheme) (instanceFor : Nat → Monotype)
    (lexical : Nat → Type) :
    (scheme.instantiate instanceFor).interpret lexical =
      scheme.body.interpret (scheme.typeArguments lexical fun name => (instanceFor name).interpret lexical) := by
  rw [scheme.instantiate_sound]
  congr 1
  funext name
  by_cases quantified : name ∈ scheme.quantified <;>
    simp [Scheme.replacement, Scheme.typeArguments, quantified, Monotype.interpret]

def Scheme.specialise (scheme : Scheme) {lexical : Nat → Type} (value : scheme.Value lexical)
    (instanceFor : Nat → Monotype) : (scheme.instantiate instanceFor).interpret lexical :=
  (scheme.instantiate_interpret instanceFor lexical).symm ▸
    value (fun name => (instanceFor name).interpret lexical)

def Scheme.freshInstance (scheme : Scheme) (next : Nat) : Monotype :=
  scheme.instantiate (fun name => .variable (next + name))

structure FreshInstance where
  type : Monotype
  next : Nat

def Scheme.allocateInstance (scheme : Scheme) (next : Nat) : FreshInstance :=
  ⟨scheme.freshInstance next, next + firstUnusedTypeName scheme.quantified⟩

theorem Scheme.allocated_name_range (scheme : Scheme) (next name : Nat)
    (quantified : name ∈ scheme.quantified) :
    next ≤ next + name ∧ next + name < (scheme.allocateInstance next).next := by
  have above := firstUnusedTypeName_above scheme.quantified name quantified
  constructor
  · omega
  · exact Nat.add_lt_add_left above next

theorem Scheme.successive_instances_disjoint (first second : Scheme) (next a b : Nat)
    (allocatedFirst : a ∈ first.quantified) (allocatedSecond : b ∈ second.quantified) :
    next + a ≠ (first.allocateInstance next).next + b := by
  have belowBoundary := (first.allocated_name_range next a allocatedFirst).2
  have fromBoundary := (second.allocated_name_range (first.allocateInstance next).next b allocatedSecond).1
  omega

theorem Scheme.fresh_replacement_disjoint (scheme : Scheme) (next : Nat) (name old : Nat)
    (quantified : name ∈ scheme.quantified) (allocated : old < next) :
    scheme.replacement (fun id => .variable (next + id)) name ≠ .variable old := by
  intro same
  have indices : next + name = old := Monotype.variable.inj (by
    simpa only [replacement, ite_eq_left quantified] using same)
  omega

end Rupam
