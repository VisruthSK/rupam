import Rupam.OrdinaryInference

namespace Rupam

inductive OrdinaryValue where
  | cell (type : Ty) (value : type.denote)
  | closure (body : OrdinarySource) (captured : List OrdinaryValue)

inductive OrdinaryEvaluates : List OrdinaryValue → OrdinarySource → OrdinaryValue → Prop where
  | literal (t : Ty) (value : t.denote) :
      OrdinaryEvaluates environment (.literal t value) (.cell t value)
  | reference (slot : Nat) (found : environment[slot]? = some value) :
      OrdinaryEvaluates environment (.reference slot) value
  | lambda : OrdinaryEvaluates environment (.lambda body) (.closure body environment)
  | apply : OrdinaryEvaluates environment function (.closure body captured) →
      OrdinaryEvaluates environment argument input →
      OrdinaryEvaluates (input :: captured) body result →
      OrdinaryEvaluates environment (.apply function argument) result

theorem OrdinaryEvaluates.deterministic (first : OrdinaryEvaluates environment term a)
    (second : OrdinaryEvaluates environment term b) : a = b := by
  induction first generalizing b with
  | literal t value => cases second; rfl
  | reference slot found =>
    cases second with
    | reference _ alsoFound => exact Option.some.inj (found.symm.trans alsoFound)
  | lambda => cases second; rfl
  | apply function argument body sameFunction sameArgument sameBody =>
    cases second with
    | apply otherFunction otherArgument otherBody =>
      have sameClosure := sameFunction otherFunction
      cases sameClosure
      have sameInput := sameArgument otherArgument
      cases sameInput
      exact sameBody otherBody

def Represents (assignment : Nat → Type) :
    (type : Monotype) → OrdinaryValue → type.interpret assignment → Prop
  | .variable _, _, _ => True
  | .cell t, source, target => source = .cell t target
  | .arrow input output, source, function =>
    ∃ body captured, source = .closure body captured ∧
      ∀ sourceArgument targetArgument, Represents assignment input sourceArgument targetArgument →
        ∃ result, OrdinaryEvaluates (sourceArgument :: captured) body result ∧
          Represents assignment output result (function targetArgument)

def OrdinaryEnvironment.Agrees (target : OrdinaryEnvironment assignment context)
    (source : List OrdinaryValue) : Prop :=
  ∀ slot {t} (found : context[slot]? = some t),
    ∃ value, source[slot]? = some value ∧ Represents assignment t value (target slot found)

theorem OrdinaryEnvironment.push_agrees (target : OrdinaryEnvironment assignment context)
    (agrees : target.Agrees source) (typed : input.interpret assignment) (untyped : OrdinaryValue)
    (represented : Represents assignment input untyped typed) :
    (target.push typed).Agrees (untyped :: source) := by
  intro slot t found
  cases slot with
  | zero =>
    have same : input = t := Option.some.inj found
    subst t
    exact ⟨untyped, rfl, represented⟩
  | succ slot =>
    obtain ⟨value, read, related⟩ := agrees slot found
    exact ⟨value, read, related⟩

theorem OrdinaryCore.evaluation_preserved (term : OrdinaryCore context t) (assignment : Nat → Type)
    (target : OrdinaryEnvironment assignment context) (source : List OrdinaryValue)
    (agrees : target.Agrees source) :
    ∃ result, OrdinaryEvaluates source term.erase result ∧
      Represents assignment t result (term.eval assignment target) := by
  induction term generalizing source with
  | literal t value =>
    exact ⟨.cell t value, .literal t value, rfl⟩
  | reference slot found =>
    obtain ⟨value, read, related⟩ := agrees slot found
    exact ⟨value, .reference slot read, related⟩
  | lambda body bodyPreserved =>
    refine ⟨.closure body.erase source, .lambda, body.erase, source, rfl, ?_⟩
    intro sourceArgument targetArgument related
    exact bodyPreserved (target.push targetArgument) (sourceArgument :: source)
      (target.push_agrees agrees targetArgument sourceArgument related)
  | apply fn argument functionPreserved argumentPreserved =>
    obtain ⟨sourceFunction, functionRan, represented⟩ := functionPreserved target source agrees
    obtain ⟨sourceArgument, argumentRan, related⟩ := argumentPreserved target source agrees
    obtain ⟨body, captured, rfl, functionAgrees⟩ := represented
    obtain ⟨result, bodyRan, resultAgrees⟩ :=
      functionAgrees sourceArgument (argument.eval assignment target) related
    exact ⟨result, .apply functionRan argumentRan bodyRan, resultAgrees⟩

theorem OrdinaryInferred.evaluation_preserved (inferred : OrdinaryInferred context term)
    (assignment : Nat → Type)
    (target : OrdinaryEnvironment assignment (context.map (Monotype.substitute inferred.assignment)))
    (source : List OrdinaryValue) (agrees : target.Agrees source) :
    ∃ result, OrdinaryEvaluates source term result ∧
      Represents assignment inferred.type result (inferred.core.eval assignment target) := by
  obtain ⟨value, ran, related⟩ := inferred.core.evaluation_preserved assignment target source agrees
  rw [inferred.erases] at ran
  exact ⟨value, ran, related⟩

theorem OrdinaryInferred.same_observation (first : OrdinaryInferred leftContext term)
    (second : OrdinaryInferred rightContext term) (source : List OrdinaryValue)
    (leftTypes rightTypes : Nat → Type)
    (left : OrdinaryEnvironment leftTypes (leftContext.map (Monotype.substitute first.assignment)))
    (right : OrdinaryEnvironment rightTypes (rightContext.map (Monotype.substitute second.assignment)))
    (leftAgrees : left.Agrees source) (rightAgrees : right.Agrees source) :
    ∃ value, OrdinaryEvaluates source term value ∧
      Represents leftTypes first.type value (first.core.eval leftTypes left) ∧
      Represents rightTypes second.type value (second.core.eval rightTypes right) := by
  obtain ⟨firstValue, firstRun, firstRepresents⟩ := first.evaluation_preserved leftTypes left source leftAgrees
  obtain ⟨secondValue, secondRun, secondRepresents⟩ :=
    second.evaluation_preserved rightTypes right source rightAgrees
  have same := firstRun.deterministic secondRun
  exact ⟨firstValue, firstRun, firstRepresents, same.symm ▸ secondRepresents⟩

end Rupam
