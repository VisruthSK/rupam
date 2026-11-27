import Rupam.Unification

namespace Rupam

-- Ordinary lexical expressions after names have been resolved to slots.
inductive OrdinarySource where
  | literal (t : Ty) (value : t.denote)
  | reference (slot : Nat)
  | lambda (body : OrdinarySource)
  | apply (function argument : OrdinarySource)

inductive OrdinaryCore : List Monotype → Monotype → Type where
  | literal (t : Ty) (value : t.denote) : OrdinaryCore context (.cell t)
  | reference (slot : Nat) (found : context[slot]? = some t) : OrdinaryCore context t
  | lambda : OrdinaryCore (input :: context) output → OrdinaryCore context (.arrow input output)
  | apply : OrdinaryCore context (.arrow input output) → OrdinaryCore context input →
      OrdinaryCore context output

def OrdinaryCore.erase : OrdinaryCore context t → OrdinarySource
  | .literal t value => .literal t value
  | .reference slot _ => .reference slot
  | .lambda body => .lambda body.erase
  | .apply fn argument => .apply fn.erase argument.erase

def OrdinaryCore.retype (typeEq : a = b) (term : OrdinaryCore context a) : OrdinaryCore context b :=
  typeEq ▸ term

theorem OrdinaryCore.erase_retype (typeEq : a = b) (term : OrdinaryCore context a) :
    (term.retype typeEq).erase = term.erase := by
  cases typeEq
  rfl

abbrev OrdinaryEnvironment (assignment : Nat → Type) (context : List Monotype) :=
  (slot : Nat) → {t : Monotype} → context[slot]? = some t → t.interpret assignment

def OrdinaryEnvironment.push (value : input.interpret assignment)
    (environment : OrdinaryEnvironment assignment context) :
    OrdinaryEnvironment assignment (input :: context)
  | 0, _, found => Option.some.inj found ▸ value
  | slot + 1, _, found => environment slot found

def OrdinaryCore.eval (assignment : Nat → Type) (environment : OrdinaryEnvironment assignment context) :
    OrdinaryCore context t → t.interpret assignment
  | .literal _ value => value
  | .reference slot found => environment slot found
  | .lambda body => fun argument => body.eval assignment (environment.push argument)
  | .apply fn argument =>
    let value := argument.eval assignment environment
    fn.eval assignment environment value

def OrdinaryCore.closedSchemeValue (core : OrdinaryCore [] t) (lexical : Nat → Type) :
    (Scheme.generalise [] t []).Value lexical :=
  fun arguments => core.eval ((Scheme.generalise [] t []).typeArguments lexical arguments)
    (fun _ {t} found => by
      change (none : Option Monotype) = some t at found
      cases found)

inductive Generates : List Monotype → OrdinarySource → Monotype → List TypeEquation → Type where
  | literal (t : Ty) (value : t.denote) : Generates context (.literal t value) (.cell t) []
  | reference (slot : Nat) (found : context[slot]? = some t) :
      Generates context (.reference slot) t []
  | lambda : Generates (input :: context) body output equations →
      Generates context (.lambda body) (.arrow input output) equations
  | apply : Generates context fn ft fNeeds → Generates context argument atype aNeeds →
      Generates context (.apply fn argument) result
        (⟨ft, .arrow atype result⟩ :: (fNeeds ++ aNeeds))

structure Generated (context : List Monotype) (source : OrdinarySource) where
  next : Nat
  type : Monotype
  equations : List TypeEquation
  derivation : Generates context source type equations

inductive OrdinaryInferenceError where
  | unknownSlot (slot : Nat)
  | unification (problem : UnificationError)

def OrdinarySource.generate (context : List Monotype) (next : Nat) :
    (source : OrdinarySource) → Except OrdinaryInferenceError (Generated context source)
  | .literal t value => .ok ⟨next, .cell t, [], .literal t value⟩
  | .reference slot =>
    match found : context[slot]? with
    | some t => .ok ⟨next, t, [], .reference slot found⟩
    | none => .error (.unknownSlot slot)
  | .lambda body => do
    let input := Monotype.variable next
    let inferred ← body.generate (input :: context) (next + 1)
    pure ⟨inferred.next, .arrow input inferred.type, inferred.equations, .lambda inferred.derivation⟩
  | .apply fn argument => do
    let f ← fn.generate context next
    let a ← argument.generate context f.next
    let result := Monotype.variable a.next
    pure ⟨a.next + 1, result, ⟨f.type, .arrow a.type result⟩ :: (f.equations ++ a.equations),
      .apply f.derivation a.derivation⟩

def OrdinarySource.allocations : OrdinarySource → Nat
  | .literal _ _ | .reference _ => 0
  | .lambda body => body.allocations + 1
  | .apply function argument => function.allocations + argument.allocations + 1

theorem OrdinarySource.generate_fresh_count (source : OrdinarySource) (context : List Monotype)
    (next : Nat) (result : Generated context source)
    (accepted : source.generate context next = .ok result) :
    result.next = next + source.allocations := by
  induction source generalizing context next with
  | literal _ _ =>
    cases accepted
    simp [allocations]
  | reference slot =>
    unfold generate at accepted
    split at accepted
    · cases accepted
      simp [allocations]
    · cases accepted
  | lambda body generatedBody =>
    unfold generate at accepted
    dsimp only at accepted
    cases inference : body.generate (.variable next :: context) (next + 1) with
    | error problem =>
      rw [inference] at accepted
      cases accepted
    | ok inferred =>
      rw [inference] at accepted
      have count := generatedBody _ _ inferred inference
      have sameNext := congrArg Generated.next (Except.ok.inj accepted)
      simp only [allocations] at *
      omega
  | apply function argument generatedFunction generatedArgument =>
    unfold generate at accepted
    cases functionResult : function.generate context next with
    | error problem =>
      rw [functionResult] at accepted
      cases accepted
    | ok f =>
      rw [functionResult] at accepted
      simp only [bind, Except.bind] at accepted
      cases argumentResult : argument.generate context f.next with
      | error problem =>
        rw [argumentResult] at accepted
        cases accepted
      | ok a =>
        rw [argumentResult] at accepted
        have nextFunction := generatedFunction _ _ f functionResult
        have nextArgument := generatedArgument _ _ a argumentResult
        have sameNext := congrArg Generated.next (Except.ok.inj accepted)
        simp only [allocations] at *
        omega

def Generates.elaborate (assignment : TypeAssignment) :
    Generates context source type equations →
    (∀ equation ∈ equations, equation.Holds assignment) →
    OrdinaryCore (context.map (Monotype.substitute assignment)) (type.substitute assignment)
  | .literal t value, _ => .literal t value
  | .reference slot found, _ =>
    .reference slot (by simp [List.getElem?_map, found])
  | .lambda body, solved => .lambda (body.elaborate assignment solved)
  | .apply fn argument, solved =>
    have compatible := solved _ List.mem_cons_self
    let left := fn.elaborate assignment (fun equation member =>
      solved equation (List.mem_cons_of_mem _ (List.mem_append_left _ member)))
    let right := argument.elaborate assignment (fun equation member =>
      solved equation (List.mem_cons_of_mem _ (List.mem_append_right _ member)))
    .apply (OrdinaryCore.retype compatible left) right

theorem Generates.elaborate_erases (derivation : Generates context source type equations)
    (assignment : TypeAssignment) (solved : ∀ equation ∈ equations, equation.Holds assignment) :
    (derivation.elaborate assignment solved).erase = source := by
  induction derivation with
  | literal t value => rfl
  | reference slot found => rfl
  | lambda body bodyErases =>
    simp only [Generates.elaborate, OrdinaryCore.erase, bodyErases]
  | @apply localContext fnSource ft fNeeds argSource atype aNeeds result fn argument fnErases argErases =>
    let forFunction : ∀ equation ∈ fNeeds, equation.Holds assignment :=
      fun equation member => solved equation (List.mem_cons_of_mem _ (List.mem_append_left _ member))
    let forArgument : ∀ equation ∈ aNeeds, equation.Holds assignment :=
      fun equation member => solved equation (List.mem_cons_of_mem _ (List.mem_append_right _ member))
    have compatible : ft.substitute assignment = (.arrow atype result : Monotype).substitute assignment :=
      solved _ List.mem_cons_self
    change OrdinarySource.apply ((fn.elaborate assignment forFunction).retype compatible).erase
      (argument.elaborate assignment forArgument).erase = _
    rw [OrdinaryCore.erase_retype compatible (fn.elaborate assignment forFunction),
      fnErases forFunction, argErases forArgument]

structure OrdinaryInferred (context : List Monotype) (source : OrdinarySource) where
  assignment : TypeAssignment
  type : Monotype
  core : OrdinaryCore (context.map (Monotype.substitute assignment)) type
  erases : core.erase = source

def OrdinarySource.infer (source : OrdinarySource) (context : List Monotype) (fuel : Nat) :
    Except OrdinaryInferenceError (OrdinaryInferred context source) := do
  let next := firstUnusedTypeName (context.flatMap Monotype.variables)
  let generated ← source.generate context next
  let solved ← (solveTypeEquations fuel generated.equations).mapError .unification
  pure ⟨solved.assignment, generated.type.substitute solved.assignment,
    generated.derivation.elaborate solved.assignment solved.sound,
    generated.derivation.elaborate_erases solved.assignment solved.sound⟩

end Rupam
