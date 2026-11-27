import Rupam.ContextInference

namespace Rupam

-- Interpret the source typing rules directly. The environment provides
-- ordinary values and deferred reads at their current consuming stage.
def ContextElaborates.sourceEval (context : ConsumptionContext)
    (environment : ContextEnvironment Ty.denote context locals) :
    ContextElaborates locals source t needs → Supplies context needs → t.denote Ty.denote context
  | .reference binding _, _ => environment binding
  | .literal _ value, _ => .broadcast (fun _ => .ok value)
  | .vector _ _ cells, _ => cells
  | .sum _ demand, _ => fun evidence input =>
    match evidence with
    | .cons _ (.cons policy .nil) =>
      input.reduce (sumInt (Requirement.policyValue demand context policy))
  | .first _ demand, _ => fun evidence input =>
    match evidence with
    | .cons _ (.cons _ (.cons policy .nil)) =>
      input.reduce (firstWithPolicy (Requirement.policyValue demand context policy))
  | .sumVector _ demand, _ => fun evidence input =>
    match evidence with
    | .cons policy .nil =>
      .broadcast (fun _ => sumInt (Requirement.policyValue demand context policy) input.toList)
  | .lambda body, _ =>
    fun forCall argument => body.sourceEval context (environment.push argument) forCall
  | .apply (functionNeeds := functionNeeds) (argumentNeeds := argumentNeeds) fn argument, available =>
    let (forFunction, rest) := available.split functionNeeds
    let (forArgument, forCall) := rest.split argumentNeeds
    let closure := fn.sourceEval context environment forFunction
    closure forCall (argument.sourceEval context environment forArgument)
  | .add (leftNeeds := leftNeeds) left right numeric, available =>
    let (forLeft, forRight) := available.split leftNeeds
    (left.sourceEval context environment forLeft).zipWith (right.sourceEval context environment forRight)
      numeric.compute
  | .conditional (testNeeds := testNeeds) (yesNeeds := yesNeeds) test yes no common, available =>
    let (forTest, forBranches) := available.split testNeeds
    let (forYes, forNo) := forBranches.split yesNeeds
    (test.sourceEval context environment forTest).ifThenElse
      ((yes.sourceEval context environment forYes).map fun value => .ok (common.leftValue value))
      ((no.sourceEval context environment forNo).map fun value => .ok (common.rightValue value))

theorem ScopedCalculation.map_success_identity (calculation : ScopedCalculation scope α) :
    calculation.map Except.ok = calculation := by
  cases calculation with
  | mk evaluate =>
    unfold ScopedCalculation.map
    apply congrArg (ScopedCalculation.mk (scope := scope) (α := α))
    funext first rest
    change (do
      let values ← evaluate first rest
      ValuesAt.collect (first :: rest) fun row member => .ok (values row member)) =
      evaluate first rest
    cases observed : evaluate first rest with
    | error failure => rfl
    | ok values =>
      have everyRow : ∀ row member, ∃ value,
          (Except.ok (values row member) : Outcome α) = .ok value :=
        fun row member => ⟨values row member, rfl⟩
      obtain ⟨collected, read⟩ := ValuesAt.collect_succeeds _ _ everyRow
      change ValuesAt.collect (first :: rest) (fun row member => .ok (values row member)) = .ok values
      rw [read]
      congr 1
      funext row member
      exact (Except.ok.inj (ValuesAt.collect_at read row member)).symm

theorem ContextElaborates.core_preserves_source (derivation : ContextElaborates locals source t needs)
    (context : ConsumptionContext) (environment : ContextEnvironment Ty.denote context locals)
    (available : Supplies context needs) :
    derivation.core.eval context environment available = derivation.sourceEval context environment available := by
  induction derivation with
  | reference binding found => rfl
  | literal type value => rfl
  | vector type size values => rfl
  | sum kind demand => rfl
  | first kind demand => rfl
  | sumVector size demand => rfl
  | lambda body agrees =>
    funext supplied argument
    exact agrees (environment.push argument) supplied
  | apply fn argument fnAgrees argAgrees =>
    simp only [core, ContextTerm.eval, sourceEval, fnAgrees, argAgrees]
  | add left right numeric leftAgrees rightAgrees =>
    simp only [core, ContextTerm.eval, sourceEval, leftAgrees, rightAgrees]
    rfl
  | @conditional _ _ testNeeds _ _ yesNeeds _ _ _ _ test yes no common testAgrees yesAgrees noAgrees =>
    simp only [core, ContextTerm.eval, sourceEval, testAgrees, yesAgrees, noAgrees]
    let (forTest, forBranches) := available.split testNeeds
    let (forYes, forNo) := forBranches.split yesNeeds
    let flags : ScopedCalculation context.scope Bool := test.sourceEval context environment forTest
    exact congrArg (fun checked => checked.ifThenElse
      ((yes.sourceEval context environment forYes).map fun value => .ok (common.leftValue value))
      ((no.sourceEval context environment forNo).map fun value => .ok (common.rightValue value)))
      (ScopedCalculation.map_success_identity flags)

def ContextElaborates.runSource (derivation : ContextElaborates locals source t needs)
    (context : ConsumptionContext) (environment : ContextEnvironment Ty.denote context locals) :
    Except Requirement (t.denote Ty.denote context) := do
  let available ← Supplies.discharge context needs
  pure (derivation.sourceEval context environment available)

theorem ContextElaborates.run_preserves_source (derivation : ContextElaborates locals source t needs)
    (context : ConsumptionContext) (environment : ContextEnvironment Ty.denote context locals) :
    derivation.core.run context environment = derivation.runSource context environment := by
  simp only [ContextTerm.run, runSource, core_preserves_source]

end Rupam
