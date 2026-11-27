import Rupam.Context
import Rupam.Pointwise

namespace Rupam

inductive ContextType (atom : Type) where
  | calculation (result : atom)
  | vectorValue (size : Nat) (element : atom)
  | function (latent : List Requirement) (input output : ContextType atom)
  deriving DecidableEq

def ContextType.denote (payload : atom → Type) (context : ConsumptionContext) :
    ContextType atom → Type
  | .calculation result => ScopedCalculation context.scope (payload result)
  | .vectorValue size element => Vector (payload element) size
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
  | vector : Vector (payload element) size → ContextTerm payload locals (.vectorValue size element) []
  | pointwise : (payload input → Outcome (payload output)) →
      ContextTerm payload locals (.function [] (.calculation input) (.calculation output)) []
  | reducer (kind : CollectionKind) (policy : PolicyDemand) :
      (MissingPolicy → List (payload input) → Outcome (payload output)) →
      ContextTerm payload locals
        (.function [.collection kind, .missing policy] (.calculation input) (.calculation output)) []
  | vectorReducer (policy : PolicyDemand) :
      (MissingPolicy → List (payload input) → Outcome (payload output)) →
      ContextTerm payload locals
        (.function [.missing policy] (.vectorValue size input) (.calculation output)) []
  | orderedReducer (kind : CollectionKind) (policy : PolicyDemand) :
      (MissingPolicy → List (payload input) → Outcome (payload output)) →
      ContextTerm payload locals
        (.function [.collection kind, .order, .missing policy] (.calculation input) (.calculation output)) []
  | mapResult : (payload input → Outcome (payload output)) →
      ContextTerm payload locals (.calculation input) needs →
      ContextTerm payload locals (.calculation output) needs
  | combine : (payload left → payload right → Outcome (payload result)) →
      ContextTerm payload locals (.calculation left) leftNeeds →
      ContextTerm payload locals (.calculation right) rightNeeds →
      ContextTerm payload locals (.calculation result) (leftNeeds ++ rightNeeds)
  | conditional : (payload predicate → Bool) →
      ContextTerm payload locals (.calculation predicate) conditionNeeds →
      ContextTerm payload locals (.calculation result) yesNeeds →
      ContextTerm payload locals (.calculation result) noNeeds →
      ContextTerm payload locals (.calculation result) (conditionNeeds ++ (yesNeeds ++ noNeeds))
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
  | .vector values, _ => values
  | .pointwise operation, _ => fun _ input => input.map operation
  | .reducer _ demand operation, _ => fun evidence input =>
    match evidence with
    | .cons _ (.cons policy .nil) =>
      input.reduce (operation (Requirement.policyValue demand context policy))
  | .vectorReducer demand operation, _ => fun evidence input =>
    match evidence with
    | .cons policy .nil =>
      .broadcast (fun _ => operation (Requirement.policyValue demand context policy) input.toList)
  | .orderedReducer _ demand operation, _ => fun evidence input =>
    match evidence with
    | .cons _ (.cons _ (.cons policy .nil)) =>
      input.reduce (operation (Requirement.policyValue demand context policy))
  | .mapResult operation input, supplies =>
    (input.eval context environment supplies).map operation
  | .combine (leftNeeds := leftNeeds) operation left right, supplies =>
    let (forLeft, forRight) := supplies.split leftNeeds
    (left.eval context environment forLeft).zipWith (right.eval context environment forRight) operation
  | .conditional (conditionNeeds := conditionNeeds) (yesNeeds := yesNeeds) test condition yes no, supplies =>
    let (forCondition, forBranches) := supplies.split conditionNeeds
    let (forYes, forNo) := forBranches.split yesNeeds
    ((condition.eval context environment forCondition).map (fun flag => .ok (test flag))).ifThenElse
      (yes.eval context environment forYes) (no.eval context environment forNo)
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

theorem ContextTerm.evidence_coherence (term : ContextTerm payload locals t needs)
    (context : ConsumptionContext) (environment : ContextEnvironment payload context locals)
    (first second : Supplies context needs) :
    term.eval context environment first = term.eval context environment second := by
  rw [first.unique second]

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

theorem ContextTerm.forwarded_application
    (fn : ContextTerm payload locals (.function latent input output) [])
    (argument : ContextTerm payload locals input argumentNeeds)
    (context : ConsumptionContext) (environment : ContextEnvironment payload context locals)
    (supplies : Supplies context (argumentNeeds ++ latent)) :
    (ContextTerm.apply (.apply (.forward latent input output) fn) argument).eval
      context environment supplies =
    (ContextTerm.apply fn argument).eval context environment supplies :=
  rfl

theorem ContextTerm.vector_reducer_reads_its_argument {atom : Type} {payload : atom → Type}
    {input output : atom} {locals : List (ContextType atom)} (demand : PolicyDemand)
    (operation : MissingPolicy → List (payload input) → Outcome (payload output))
    (context : ConsumptionContext) (environment : ContextEnvironment payload context locals)
    (policy : (Requirement.missing demand).Evidence context) (values : Vector (payload input) size)
    (first : context.scope.Member) (rest : List context.scope.Member) :
    (((ContextTerm.vectorReducer (size := size) demand operation).eval context environment .nil)
      (.cons policy .nil) values).run (first :: rest) = (do
        let result ← operation (Requirement.policyValue demand context policy) values.toList
        pure fun _ _ => result) := rfl

end Rupam
