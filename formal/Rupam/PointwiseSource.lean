import Rupam.Elaboration
import Rupam.VectorCalls
import Init.Data.Vector.OfFn

namespace Rupam

-- The parameters of a resolved first-order helper are unannotated slots.
inductive PointwiseSource where
  | parameter (slot : Nat)
  | literal (type : Ty) (value : type.denote)
  | add (left right : PointwiseSource)
  | multiply (left right : PointwiseSource)
  | apply (function argument : PointwiseSource)
  | conditional (condition yes no : PointwiseSource)

structure PointwiseFunction where
  arity : Nat
  body : PointwiseSource

private abbrev boolTy := Ty.required .bool

private def Parameter.lookup : (types : List Ty) → Nat → Option (Σ t, Parameter types t)
  | [], _ => none
  | t :: _, 0 => some ⟨t, .first⟩
  | _ :: rest, slot + 1 => (lookup rest slot).map fun ⟨t, parameter⟩ => ⟨t, .next parameter⟩

-- The typing derivation records only source rules. Its evaluator does not run Core.
inductive PointwiseElaborates (parameters : List Ty) : PointwiseSource → Ty → Type where
  | parameter (found : Parameter parameters t)
      (resolved : Parameter.lookup parameters slot = some ⟨t, found⟩) :
      PointwiseElaborates parameters (.parameter slot) t
  | literal (t : Ty) (value : t.denote) : PointwiseElaborates parameters (.literal t value) t
  | add : PointwiseElaborates parameters left a → PointwiseElaborates parameters right b →
      IntOperands a b t → PointwiseElaborates parameters (.add left right) t
  | multiply : PointwiseElaborates parameters left a → PointwiseElaborates parameters right b →
      IntOperands a b t → PointwiseElaborates parameters (.multiply left right) t
  | apply : PointwiseElaborates parameters function (.function input output) →
      PointwiseElaborates parameters argument input →
      PointwiseElaborates parameters (.apply function argument) output
  | conditional : PointwiseElaborates parameters condition boolTy →
      PointwiseElaborates parameters yes a → PointwiseElaborates parameters no b →
      CommonType a b t → PointwiseElaborates parameters (.conditional condition yes no) t

private def IntOperands.addBody : IntOperands a b t →
    ScalarBody parameters a → ScalarBody parameters b → ScalarBody parameters t
  | .required, left, right => .add left right
  | .optionalLeft, left, right => .addOptional left (.optional right)
  | .optionalRight, left, right => .addOptional (.optional left) right
  | .optionalBoth, left, right => .addOptional left right

private def IntOperands.multiplyBody : IntOperands a b t →
    ScalarBody parameters a → ScalarBody parameters b → ScalarBody parameters t
  | .required, left, right => .multiply left right
  | .optionalLeft, left, right => .multiplyOptional left (.optional right)
  | .optionalRight, left, right => .multiplyOptional (.optional left) right
  | .optionalBoth, left, right => .multiplyOptional left right

private def IntOperands.multiplyValues : IntOperands a b t → a.denote → b.denote → Outcome t.denote
  | .required, left, right => multiplyInt left right
  | .optionalLeft, left, right => lift₂ multiplyInt left (some right)
  | .optionalRight, left, right => lift₂ multiplyInt (some left) right
  | .optionalBoth, left, right => lift₂ multiplyInt left right

private def CommonType.leftBody : CommonType a b t → ScalarBody parameters a → ScalarBody parameters t
  | .same, value | .leftOptional, value => value
  | .rightOptional, value => .optional value

private def CommonType.rightBody : CommonType a b t → ScalarBody parameters b → ScalarBody parameters t
  | .same, value | .rightOptional, value => value
  | .leftOptional, value => .optional value

def PointwiseElaborates.body : PointwiseElaborates parameters source t → ScalarBody parameters t
  | .parameter found _ => .arg found
  | .literal _ value => .literal value
  | .add left right numeric => numeric.addBody left.body right.body
  | .multiply left right numeric => numeric.multiplyBody left.body right.body
  | .apply function argument => .apply function.body argument.body
  | .conditional condition yes no common =>
    .ifThenElse condition.body (common.leftBody yes.body) (common.rightBody no.body)

def PointwiseElaborates.eval (arguments : ArgumentValues parameters) :
    PointwiseElaborates parameters source t → Outcome t.denote
  | .parameter found _ => .ok (arguments found)
  | .literal _ value => .ok value
  | .add left right numeric => do numeric.compute (← left.eval arguments) (← right.eval arguments)
  | .multiply left right numeric => do
    numeric.multiplyValues (← left.eval arguments) (← right.eval arguments)
  | .apply function argument => do
    let invoke ← function.eval arguments
    invoke (← argument.eval arguments)
  | .conditional condition yes no common => do
    choose (← condition.eval arguments)
      (fun _ => common.leftValue <$> yes.eval arguments)
      (fun _ => common.rightValue <$> no.eval arguments)

theorem PointwiseElaborates.body_correct (checked : PointwiseElaborates parameters source t)
    (arguments : ArgumentValues parameters) :
    checked.body.eval (fun p => .ok (arguments p)) = checked.eval arguments := by
  induction checked with
  | parameter => rfl
  | literal => rfl
  | add left right numeric leftCorrect rightCorrect =>
    cases numeric <;> simp only [body, IntOperands.addBody, ScalarBody.eval, eval,
      leftCorrect, rightCorrect, IntOperands.compute] <;>
      cases left.eval arguments <;> cases right.eval arguments <;> rfl
  | multiply left right numeric leftCorrect rightCorrect =>
    cases numeric <;> simp only [body, IntOperands.multiplyBody, ScalarBody.eval, eval,
      leftCorrect, rightCorrect, IntOperands.multiplyValues] <;>
      cases left.eval arguments <;> cases right.eval arguments <;> rfl
  | apply _ _ functionCorrect argumentCorrect =>
    simp only [body, ScalarBody.eval, eval, functionCorrect, argumentCorrect]
  | conditional condition yes no common conditionCorrect yesCorrect noCorrect =>
    cases common <;> simp only [body, CommonType.leftBody, CommonType.rightBody, ScalarBody.eval,
      eval, conditionCorrect, yesCorrect, noCorrect] <;>
      cases condition.eval arguments with
      | error _ => rfl
      | ok flag => cases flag <;> first | rfl | cases yes.eval arguments <;> rfl |
          cases no.eval arguments <;> rfl

private def PointwiseSource.check (parameters : List Ty) :
    (source : PointwiseSource) → Option (Σ t, PointwiseElaborates parameters source t)
  | .parameter slot => do
    match resolved : Parameter.lookup parameters slot with
    | none => none
    | some ⟨t, binding⟩ => some ⟨t, .parameter binding resolved⟩
  | .literal t value => some ⟨t, .literal t value⟩
  | .add left right => do
    let ⟨a, l⟩ ← left.check parameters
    let ⟨b, r⟩ ← right.check parameters
    let ⟨t, numeric⟩ ← IntOperands.infer a b
    pure ⟨t, .add l r numeric⟩
  | .multiply left right => do
    let ⟨a, l⟩ ← left.check parameters
    let ⟨b, r⟩ ← right.check parameters
    let ⟨t, numeric⟩ ← IntOperands.infer a b
    pure ⟨t, .multiply l r numeric⟩
  | .apply function argument => do
    let ⟨.function input output, fn⟩ ← function.check parameters | none
    let ⟨supplied, arg⟩ ← argument.check parameters
    if exact : supplied = input then some ⟨output, .apply fn (exact ▸ arg)⟩ else none
  | .conditional condition yes no => do
    let ⟨test, c⟩ ← condition.check parameters
    let ⟨a, y⟩ ← yes.check parameters
    let ⟨b, n⟩ ← no.check parameters
    if hc : test = boolTy then do
      let ⟨t, common⟩ ← CommonType.infer a b
      pure ⟨t, .conditional (hc ▸ c) y n common⟩
    else none

private theorem PointwiseElaborates.check_agrees
    (checked : PointwiseElaborates parameters source type) :
    source.check parameters = some ⟨type, checked⟩ := by
  induction checked with
  | parameter _ resolved =>
    unfold PointwiseSource.check
    split
    · rename_i rejected
      rw [rejected] at resolved
      cases resolved
    · rename_i found accepted
      cases accepted.symm.trans resolved
      rfl
  | literal => rfl
  | add _ _ numeric leftCorrect rightCorrect =>
    simp [PointwiseSource.check, leftCorrect, rightCorrect, bind, Option.bind,
      numeric.infer_complete, pure]
  | multiply _ _ numeric leftCorrect rightCorrect =>
    simp [PointwiseSource.check, leftCorrect, rightCorrect, bind, Option.bind,
      numeric.infer_complete, pure]
  | apply _ _ functionCorrect argumentCorrect =>
    simp [PointwiseSource.check, functionCorrect, argumentCorrect, bind, Option.bind]
  | conditional _ _ _ common conditionCorrect yesCorrect noCorrect =>
    simp [PointwiseSource.check, conditionCorrect, yesCorrect, noCorrect, bind, Option.bind,
      common.infer_complete, pure]

-- Reference search over the two admissible views of a collection. Each slot
-- tries the whole type before its element type, without materialising values.
private def findSpecialization (actuals : List Ty) (accept : List Ty → Option α) : Option α :=
  match actuals with
  | [] => accept []
  | actual :: rest =>
    match findSpecialization rest (fun tail => accept (actual :: tail)) with
    | some result => some result
    | none =>
      match actual with
      | .vector _ element | .column _ element =>
        findSpecialization rest (fun tail => accept (element :: tail))
      | .required _ | .optional _ | .function .. => none

private theorem findSpecialization_complete (passing : ArgumentPassings actuals parameters shapes)
    (accept : List Ty → Option α) (accepted : (accept parameters).isSome = true) :
    (findSpecialization actuals accept).isSome = true := by
  induction passing generalizing accept with
  | nil => exact accepted
  | cons head _ tailComplete =>
    cases head with
    | whole =>
      have found := tailComplete (fun tail => accept (_ :: tail)) accepted
      simp only [findSpecialization]
      split
      · rfl
      · rename_i rejected
        simp [rejected] at found
    | vector =>
      simp only [findSpecialization]
      split
      · rfl
      · exact tailComplete (fun tail => accept (_ :: tail)) accepted
    | column =>
      simp only [findSpecialization]
      split
      · rfl
      · exact tailComplete (fun tail => accept (_ :: tail)) accepted

private theorem findSpecialization_prefers_whole (accept : List Ty → Option α)
    (wholeFits : accept actuals = some result) :
    findSpecialization actuals accept = some result := by
  induction actuals generalizing accept with
  | nil => exact wholeFits
  | cons actual rest tailPrefers =>
    have found := tailPrefers (fun tail => accept (actual :: tail)) wholeFits
    simp [findSpecialization, found]

private def CheckedValue.argumentType (value : CheckedValue) : Ty :=
  value.shape.valueType value.type

private def CheckedValue.materialize (value : CheckedValue) : Outcome value.argumentType.denote :=
  match value with
  | ⟨.scalar, _, expr⟩ => expr.eval ()
  | ⟨.vector _, _, expr⟩ => Vector.ofFnM expr.eval
  | ⟨.column _, _, expr⟩ => Vector.get <$> Vector.ofFnM expr.eval

private def Inferred.checkedValue (inferred : Inferred context source) : CheckedValue :=
  ⟨inferred.shape, inferred.type, inferred.derivation.core⟩

inductive PointwiseArguments (context : SourceContext) : List SourceExpr → List Ty → Type where
  | nil : PointwiseArguments context [] []
  | cons (head : Inferred context source) : PointwiseArguments context rest types →
      PointwiseArguments context (source :: rest) (head.checkedValue.argumentType :: types)

private def PointwiseArguments.infer (context : SourceContext) :
    (arguments : List SourceExpr) → Except ElaborationError (Σ actuals, PointwiseArguments context arguments actuals)
  | [] => .ok ⟨[], .nil⟩
  | head :: rest => do
    let inferred ← head.infer context
    let ⟨_, tail⟩ ← infer context rest
    pure ⟨_, .cons inferred tail⟩

private theorem PointwiseArguments.infer_agrees
    (checked : PointwiseArguments context arguments actuals) :
    infer context arguments = .ok ⟨actuals, checked⟩ := by
  induction checked with
  | nil => rfl
  | cons head tail tailCorrect =>
    cases head with
    | mk shape type derivation =>
      simp [infer, derivation.infer_agrees, tailCorrect, bind, Except.bind, pure, Except.pure]

private def PointwiseArguments.sources (checked : PointwiseArguments context arguments actuals) :
    ArgumentSources actuals :=
  match checked with
  | .nil => fun param => nomatch param
  | .cons head tail => fun
    | .first => fun _ => head.checkedValue.materialize
    | .next param => tail.sources param

structure InferredPointwiseCall (context : SourceContext) (helper : PointwiseFunction)
    (arguments : List SourceExpr) where
  arityMatches : arguments.length = helper.arity
  actuals : List Ty
  inputs : PointwiseArguments context arguments actuals
  parameters : List Ty
  shape : Shape
  type : Ty
  derivation : PointwiseElaborates parameters helper.body type
  call : CheckedCall actuals parameters shape

inductive PointwiseCallError where
  | arity (expected actual : Nat)
  | argument (error : ElaborationError)
  | noSpecialization

private def PointwiseFunction.checkSpecialization (helper : PointwiseFunction)
    (arityMatches : arguments.length = helper.arity)
    (inputs : PointwiseArguments context arguments actuals) (parameters : List Ty) :
    Option (InferredPointwiseCall context helper arguments) := do
  let ⟨type, derivation⟩ ← helper.body.check parameters
  let ⟨shape, call⟩ ← CheckedCall.infer actuals parameters
  pure ⟨arityMatches, actuals, inputs, parameters, shape, type, derivation, call⟩

private theorem InferredPointwiseCall.check_agrees
    (witness : InferredPointwiseCall context helper arguments) :
    helper.checkSpecialization witness.arityMatches witness.inputs witness.parameters =
      some witness := by
  cases witness with
  | mk arityMatches actuals inputs parameters shape type derivation call =>
    simp [PointwiseFunction.checkSpecialization, derivation.check_agrees, call.infer_complete,
      bind, Option.bind, pure]

def PointwiseFunction.inferCall (helper : PointwiseFunction) (context : SourceContext)
    (arguments : List SourceExpr) :
    Except PointwiseCallError (InferredPointwiseCall context helper arguments) :=
  if arityMatches : arguments.length = helper.arity then do
    let ⟨actuals, inputs⟩ ← (PointwiseArguments.infer context arguments).mapError .argument
    let some inferred := findSpecialization actuals (helper.checkSpecialization arityMatches inputs)
      | .error .noSpecialization
    pure inferred
  else .error (.arity helper.arity arguments.length)

theorem PointwiseFunction.inferCall_succeeds_iff (helper : PointwiseFunction)
    (context : SourceContext) (arguments : List SourceExpr) :
    (∃ inferred, helper.inferCall context arguments = .ok inferred) ↔
      Nonempty (InferredPointwiseCall context helper arguments) := by
  constructor
  · intro ⟨inferred, _⟩
    exact ⟨inferred⟩
  · intro ⟨witness⟩
    have valid : (helper.checkSpecialization witness.arityMatches witness.inputs
        witness.parameters).isSome = true := by
      simp [witness.check_agrees]
    have found := findSpecialization_complete witness.call.passing _ valid
    unfold inferCall
    rw [dite_eq_left witness.arityMatches, witness.inputs.infer_agrees]
    simp only [Except.mapError, bind, Except.bind]
    cases searched : findSpecialization witness.actuals
        (helper.checkSpecialization witness.arityMatches witness.inputs) with
    | none => simp [searched] at found
    | some inferred => exact ⟨inferred, rfl⟩

theorem PointwiseFunction.inferCall_prefers_whole
    (witness : InferredPointwiseCall context helper arguments)
    (allWhole : witness.parameters = witness.actuals) :
    helper.inferCall context arguments = .ok witness := by
  have wholeFits := witness.check_agrees
  rw [allWhole] at wholeFits
  have found := findSpecialization_prefers_whole _ wholeFits
  unfold inferCall
  rw [dite_eq_left witness.arityMatches, witness.inputs.infer_agrees]
  simp only [Except.mapError, bind, Except.bind, found]
  rfl

def InferredPointwiseCall.run (inferred : InferredPointwiseCall context helper arguments)
    (demanded : List (Position inferred.shape)) : Outcome (List inferred.type.denote) :=
  inferred.call.run inferred.derivation.body inferred.inputs.sources demanded

theorem InferredPointwiseCall.run_preserves_source
    (inferred : InferredPointwiseCall context helper arguments)
    (demanded : List (Position inferred.shape)) :
    inferred.run demanded = (do
      let values ← ArgumentSources.evaluate inferred.inputs.sources
      let call := inferred.call
      demanded.mapM fun position =>
        inferred.derivation.eval (call.passing.valuesAt call.alignment values position)) := by
  simp only [run, CheckedCall.run_eq, PointwiseElaborates.body_correct]

def InferredPointwiseCall.inScope {scope : EvaluationScope}
    (inferred : InferredPointwiseCall context helper arguments)
    (compatible : inferred.shape = scope.shape) : ScopedCalculation scope inferred.type.denote :=
  let call : CheckedCall inferred.actuals inferred.parameters scope.shape := compatible ▸ inferred.call
  call.scopedCall inferred.derivation.body inferred.inputs.sources

theorem InferredPointwiseCall.scoped_preserves_source {scope : EvaluationScope}
    (inferred : InferredPointwiseCall context helper arguments)
    (compatible : inferred.shape = scope.shape) (demanded : List scope.Member) :
    (ValuesAt.toList <$> (inferred.inScope compatible).run demanded) =
      (match demanded with
      | [] => .ok []
      | _ :: _ => do
        let values ← ArgumentSources.evaluate inferred.inputs.sources
        demanded.mapM fun row =>
          let position := cast (congrArg Position compatible.symm) row.val
          inferred.derivation.eval
            (inferred.call.passing.valuesAt inferred.call.alignment values position)) := by
  unfold inScope
  rw [inferred.call.scopedCall_transport compatible]
  cases demanded with
  | nil => rfl
  | cons first rest =>
    change inferred.run ((first :: rest).map _) = _
    rw [inferred.run_preserves_source]
    simp only [List.mapM_map]
    rfl
theorem CheckedVectorCall.run_preserves_source (checked : CheckedVectorCall plan)
    (body : PointwiseElaborates parameters source t) (sources : VectorCallSources parameters plan)
    (demanded : (shape : Shape) → List (Position shape)) :
    checked.run body.body sources demanded = (do
      let values ← sources.evaluate
      let inputs := values.materialize
      match inputs.align with
      | none => .error .vectorLengthMismatch
      | some ⟨shape, call⟩ =>
        let results ← (demanded shape).mapM fun position =>
          body.eval (call.passing.valuesAt call.alignment inputs.values position)
        pure (shape, results)) := by
  simp only [CheckedVectorCall.run_preserves_pointwise, PointwiseElaborates.body_correct]
  rfl

end Rupam
