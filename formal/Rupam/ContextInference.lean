import Rupam.HigherOrder
import Rupam.Elaboration

namespace Rupam

def Local.lookup : (locals : List (ContextType atom)) → Nat → Option (Σ t, Local locals t)
  | [], _ => none
  | t :: _, 0 => some ⟨t, .first⟩
  | _ :: rest, slot + 1 => (lookup rest slot).map (fun found => ⟨found.1, .next found.2⟩)

inductive ContextualSource where
  | reference (slot : Nat)
  | literal (type : Ty) (value : type.denote)
  | vector (element : Ty) (size : Nat) (values : Vector element.denote size)
  | sum (scope : CollectionKind) (policy : PolicyDemand)
  | first (scope : CollectionKind) (policy : PolicyDemand)
  | sumVector (size : Nat) (policy : PolicyDemand)
  | lambda (parameter : ContextType Ty) (body : ContextualSource)
  | apply (function argument : ContextualSource)
  | add (left right : ContextualSource)
  | conditional (test yes no : ContextualSource)

private def optionalIntCalculation : ContextType Ty := .calculation (.optional .int)

inductive ContextElaborates :
    List (ContextType Ty) → ContextualSource → ContextType Ty → List Requirement → Type where
  | reference (binding : Local locals t) (found : Local.lookup locals slot = some ⟨t, binding⟩) :
      ContextElaborates locals (.reference slot) t []
  | literal (t : Ty) (value : t.denote) :
      ContextElaborates locals (.literal t value) (.calculation t) []
  | vector (t : Ty) (size : Nat) (values : Vector t.denote size) :
      ContextElaborates locals (.vector t size values) (.vectorValue size t) []
  | sum (kind : CollectionKind) (policy : PolicyDemand) :
      ContextElaborates locals (.sum kind policy)
        (.function [.collection kind, .missing policy] optionalIntCalculation optionalIntCalculation) []
  | first (kind : CollectionKind) (policy : PolicyDemand) :
      ContextElaborates locals (.first kind policy)
        (.function [.collection kind, .order, .missing policy] optionalIntCalculation optionalIntCalculation) []
  | sumVector (size : Nat) (policy : PolicyDemand) :
      ContextElaborates locals (.sumVector size policy)
        (.function [.missing policy] (.vectorValue size (.optional .int)) optionalIntCalculation) []
  | lambda : ContextElaborates (input :: locals) body output latent →
      ContextElaborates locals (.lambda input body) (.function latent input output) []
  | apply : ContextElaborates locals function (.function latent input output) functionNeeds →
      ContextElaborates locals argument input argumentNeeds →
      ContextElaborates locals (.apply function argument) output (functionNeeds ++ (argumentNeeds ++ latent))
  | add : ContextElaborates locals left (.calculation a) leftNeeds →
      ContextElaborates locals right (.calculation b) rightNeeds → IntOperands a b result →
      ContextElaborates locals (.add left right) (.calculation result) (leftNeeds ++ rightNeeds)
  | conditional : ContextElaborates locals test (.calculation (.required .bool)) testNeeds →
      ContextElaborates locals yes (.calculation a) yesNeeds →
      ContextElaborates locals no (.calculation b) noNeeds → CommonType a b result →
      ContextElaborates locals (.conditional test yes no) (.calculation result)
        (testNeeds ++ (yesNeeds ++ noNeeds))

def ContextElaborates.core : ContextElaborates locals source t needs → ContextTerm Ty.denote locals t needs
  | .reference binding _ => .variable binding
  | .literal _ value => .scalar value
  | .vector _ _ values => .vector values
  | .sum kind policy => .reducer kind policy sumInt
  | .first kind policy => .orderedReducer kind policy firstWithPolicy
  | .sumVector _ policy => .vectorReducer policy sumInt
  | .lambda body => .lambda body.core
  | .apply fn argument => .apply fn.core argument.core
  | .add left right operands => .combine operands.compute left.core right.core
  | .conditional test yes no common =>
    .conditional id test.core
      (.mapResult (fun value => .ok (common.leftValue value)) yes.core)
      (.mapResult (fun value => .ok (common.rightValue value)) no.core)

structure ContextInferred (locals : List (ContextType Ty)) (source : ContextualSource) where
  type : ContextType Ty
  needs : List Requirement
  derivation : ContextElaborates locals source type needs

inductive ContextInferenceError where
  | unbound (slot : Nat)
  | notCallable (type : ContextType Ty)
  | incompatibleArgument (expected actual : ContextType Ty)
  | nonBooleanGuard (type : ContextType Ty)
  | incompatibleBranches (left right : ContextType Ty)
  | expectedNumbers (left right : ContextType Ty)

def ContextualSource.infer (locals : List (ContextType Ty)) :
    (source : ContextualSource) → Except ContextInferenceError (ContextInferred locals source)
  | .reference slot =>
    match found : Local.lookup locals slot with
    | none => .error (.unbound slot)
    | some ⟨type, binding⟩ => .ok ⟨type, [], .reference binding found⟩
  | .literal type value => .ok ⟨.calculation type, [], .literal type value⟩
  | .vector type size values => .ok ⟨.vectorValue size type, [], .vector type size values⟩
  | .sum kind policy => .ok ⟨_, [], .sum kind policy⟩
  | .first kind policy => .ok ⟨_, [], .first kind policy⟩
  | .sumVector size policy => .ok ⟨_, [], .sumVector size policy⟩
  | .lambda parameter body => do
    let inferred ← body.infer (parameter :: locals)
    pure ⟨.function inferred.needs parameter inferred.type, [], .lambda inferred.derivation⟩
  | .apply fn argument => do
    let f ← fn.infer locals
    match f with
    | ⟨.function latent input result, needs, derivation⟩ => do
      let a ← argument.infer locals
      if compatible : a.type = input then
        .ok ⟨result, needs ++ (a.needs ++ latent), .apply derivation (compatible ▸ a.derivation)⟩
      else
        .error (.incompatibleArgument input a.type)
    | nonFunction => .error (.notCallable nonFunction.type)
  | .add left right => do
    let a ← left.infer locals
    let b ← right.infer locals
    match a, b with
    | ⟨.calculation left, leftNeeds, leftDerivation⟩,
      ⟨.calculation right, rightNeeds, rightDerivation⟩ =>
      match IntOperands.infer left right with
      | some ⟨type, operands⟩ =>
        .ok ⟨.calculation type, leftNeeds ++ rightNeeds, .add leftDerivation rightDerivation operands⟩
      | none => .error (.expectedNumbers (.calculation left) (.calculation right))
    | leftCandidate, rightCandidate => .error (.expectedNumbers leftCandidate.type rightCandidate.type)
  | .conditional test yes no => do
    let condition ← test.infer locals
    if boolean : condition.type = .calculation (.required .bool) then
      let a ← yes.infer locals
      let b ← no.infer locals
      match a, b with
      | ⟨.calculation left, leftNeeds, leftDerivation⟩,
        ⟨.calculation right, rightNeeds, rightDerivation⟩ =>
        match CommonType.infer left right with
        | some ⟨type, common⟩ =>
          .ok ⟨.calculation type, condition.needs ++ (leftNeeds ++ rightNeeds),
            .conditional (boolean ▸ condition.derivation) leftDerivation rightDerivation common⟩
        | none => .error (.incompatibleBranches (.calculation left) (.calculation right))
      | leftCandidate, rightCandidate => .error (.incompatibleBranches leftCandidate.type rightCandidate.type)
    else .error (.nonBooleanGuard condition.type)

theorem ContextElaborates.infer_agrees (derivation : ContextElaborates locals source t needs) :
    source.infer locals = .ok ⟨t, needs, derivation⟩ := by
  induction derivation with
  | reference binding found =>
    unfold ContextualSource.infer
    split
    · rename_i absent
      rw [absent] at found
      cases found
    · rename_i type slot resolved
      have same := Option.some.inj (resolved.symm.trans found)
      cases same
      rfl
  | literal t value => rfl
  | vector t size values => rfl
  | sum kind policy => rfl
  | first kind policy => rfl
  | sumVector size policy => rfl
  | lambda body agrees =>
    rw [ContextualSource.infer, agrees]
    rfl
  | apply fn argument fnAgrees argumentAgrees =>
    rw [ContextualSource.infer, fnAgrees, argumentAgrees]
    simp [bind, Except.bind]
  | add left right operands leftAgrees rightAgrees =>
    rw [ContextualSource.infer, leftAgrees, rightAgrees]
    simp [bind, Except.bind, operands.infer_complete]
  | conditional test yes no common testAgrees yesAgrees noAgrees =>
    rw [ContextualSource.infer, testAgrees]
    simp [bind, Except.bind, yesAgrees, noAgrees, common.infer_complete]

theorem ContextElaborates.coherent (first : ContextElaborates locals source firstType firstNeeds)
    (second : ContextElaborates locals source secondType secondNeeds) :
    (⟨firstType, firstNeeds, first⟩ : ContextInferred locals source) = ⟨secondType, secondNeeds, second⟩ :=
  Except.ok.inj (first.infer_agrees.symm.trans second.infer_agrees)

theorem ContextInferred.requires_exactly (inferred : ContextInferred locals source)
    (context : ConsumptionContext) (environment : ContextEnvironment Ty.denote context locals) :
    (∃ result, inferred.derivation.core.run context environment = .ok result) ↔
      Nonempty (Supplies context inferred.needs) :=
  inferred.derivation.core.run_checks_requirements context environment

end Rupam
