import Rupam.Context
import Rupam.Pointwise

namespace Rupam

inductive ContextType (atom : Type) where
  | calculation (result : atom)
  | function (latent : List Requirement) (input output : ContextType atom)

def ContextType.denote (payload : atom → Type) (context : ConsumptionContext) :
    ContextType atom → Type
  | .calculation result => ScopedCalculation context.scope (payload result)
  | .function latent input output =>
    Supplies context latent → input.denote payload context → output.denote payload context

inductive Local : List (ContextType atom) → ContextType atom → Type where
  | first : Local (t :: rest) t
  | next : Local rest t → Local (other :: rest) t

abbrev ContextEnvironment (payload : atom → Type) (context : ConsumptionContext)
    (locals : List (ContextType atom)) :=
  ∀ {t}, Local locals t → t.denote payload context

def ContextEnvironment.push (value : input.denote payload context)
    (environment : ContextEnvironment payload context locals) :
    ContextEnvironment payload context (input :: locals)
  | _, .first => value
  | _, .next binding => environment binding

-- A lambda has no immediate requirements. Its body needs its latent
-- requirements when called, even if the function has been passed as a value.
inductive ContextTerm (payload : atom → Type) :
    List (ContextType atom) → ContextType atom → List Requirement → Type where
  | variable : Local locals t → ContextTerm payload locals t []
  | scalar : payload result → ContextTerm payload locals (.calculation result) []
  | pointwise : (payload input → Outcome (payload output)) →
      ContextTerm payload locals (.function [] (.calculation input) (.calculation output)) []
  | reducer (kind : CollectionKind) (policy : PolicyDemand) :
      (MissingPolicy → List (payload input) → Outcome (payload output)) →
      ContextTerm payload locals
        (.function [.collection kind, .missing policy] (.calculation input) (.calculation output)) []
  | lambda : ContextTerm payload (input :: locals) output latent →
      ContextTerm payload locals (.function latent input output) []
  | apply : ContextTerm payload locals (.function latent input output) functionNeeds →
      ContextTerm payload locals input argumentNeeds →
      ContextTerm payload locals output (functionNeeds ++ (argumentNeeds ++ latent))

def ContextTerm.eval (context : ConsumptionContext)
    (environment : ContextEnvironment payload context locals) :
    ContextTerm payload locals t needs → Supplies context needs → t.denote payload context
  | .variable binding, _ => environment binding
  | .scalar value, _ => .broadcast (fun _ => .ok value)
  | .pointwise operation, _ => fun _ input => input.map operation
  | .reducer _ demand operation, _ => fun evidence input =>
    match evidence with
    | .cons _ (.cons policy .nil) =>
      input.reduce (operation (Requirement.policyValue demand context policy))
  | .lambda body, _ =>
    fun latent argument => body.eval context (environment.push argument) latent
  | .apply (functionNeeds := functionNeeds) (argumentNeeds := argumentNeeds) fn argument, supplies =>
    let (forFunction, rest) := supplies.split functionNeeds
    let (forArgument, forCall) := rest.split argumentNeeds
    (fn.eval context environment forFunction) forCall (argument.eval context environment forArgument)

def ContextTerm.run (term : ContextTerm payload locals t needs) (context : ConsumptionContext)
    (environment : ContextEnvironment payload context locals) :
    Except Requirement (t.denote payload context) := do
  let supplies ← Supplies.discharge context needs
  pure (term.eval context environment supplies)

theorem ContextTerm.run_checks_requirements (term : ContextTerm payload locals t needs)
    (context : ConsumptionContext) (environment : ContextEnvironment payload context locals) :
    (∃ result, term.run context environment = .ok result) ↔ Nonempty (Supplies context needs) := by
  constructor
  · intro ⟨result, evaluated⟩
    unfold ContextTerm.run at evaluated
    cases checked : Supplies.discharge context needs with
    | error requirement =>
      rw [checked] at evaluated
      cases evaluated
    | ok supplies => exact ⟨supplies⟩
  · intro ⟨evidence⟩
    obtain ⟨checked, accepted⟩ := evidence.discharge_complete
    refine ⟨term.eval context environment checked, ?_⟩
    unfold ContextTerm.run
    rw [accepted]
    rfl

theorem ContextTerm.apply_lambda (context : ConsumptionContext)
    (environment : ContextEnvironment payload context locals)
    (body : ContextTerm payload (input :: locals) output latent)
    (argument : ContextTerm payload locals input argumentNeeds)
    (supplies : Supplies context (argumentNeeds ++ latent)) :
    (ContextTerm.apply (.lambda body) argument).eval context environment supplies =
      let (forArgument, forCall) := supplies.split argumentNeeds
      body.eval context (environment.push (argument.eval context environment forArgument)) forCall :=
  rfl

-- The checked body of `fn(f) => fn(x) => f(x)`.
def ContextTerm.forward (latent : List Requirement) (input output : ContextType atom) :
    ContextTerm payload locals
      (.function [] (.function latent input output) (.function latent input output)) [] :=
  .lambda (.lambda (.apply (.variable (.next .first)) (.variable .first)))

theorem ContextTerm.forward_preserves_call (context : ConsumptionContext)
    (environment : ContextEnvironment payload context locals)
    (function : (ContextType.function latent input output).denote payload context)
    (argument : input.denote payload context) (supplies : Supplies context latent) :
    ((ContextTerm.forward latent input output).eval context environment .nil .nil function)
      supplies argument = function supplies argument :=
  rfl

end Rupam
