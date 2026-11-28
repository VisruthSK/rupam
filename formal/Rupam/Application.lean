import Rupam.Scope
import Rupam.Alignment

namespace Rupam

variable {scope : EvaluationScope}

inductive ScopedArguments (scope : EvaluationScope) : List Ty → Type where
  | nil : ScopedArguments scope []
  | cons : ScopedCalculation scope t.denote → ScopedArguments scope rest →
      ScopedArguments scope (t :: rest)

abbrev ArgumentValues (parameters : List Ty) :=
  ∀ {t}, Parameter parameters t → t.denote

def ArgumentValues.nil : ArgumentValues [] := fun parameter => nomatch parameter

def ArgumentValues.cons (head : t.denote) (tail : ArgumentValues rest) :
    ArgumentValues (t :: rest)
  | _, .first => head
  | _, .next param => tail param

-- Ordinary arguments are evaluated in order. The body receives values even
-- if a parameter is used more than once or only under a guard.
def ScopedArguments.run (rows : List scope.Member) :
    ScopedArguments scope parameters → Outcome (ValuesAt rows (ArgumentValues parameters))
  | .nil => .ok (fun _ _ _ param => nomatch param)
  | .cons head tail => do
    let first ← head.run rows
    let rest ← tail.run rows
    pure fun row member => ArgumentValues.cons (first row member) (rest row member)

def ScopedCalculation.call (body : ScalarBody parameters t)
    (arguments : ScopedArguments scope parameters) : ScopedCalculation scope t.denote :=
  ⟨fun first rest => do
    let rows := first :: rest
    let values ← arguments.run rows
    ValuesAt.collect rows fun row member => body.eval (fun param => .ok (values row member param))⟩

theorem ScopedCalculation.call_at (body : ScalarBody parameters t)
    (arguments : ScopedArguments scope parameters) (rows : List scope.Member)
    {values : ValuesAt rows (ArgumentValues parameters)} {output : ValuesAt rows t.denote}
    (argumentsRead : arguments.run rows = .ok values)
    (called : (call body arguments).run rows = .ok output)
    (row : scope.Member) (member : row ∈ rows) :
    body.eval (fun param => .ok (values row member param)) = .ok (output row member) := by
  cases rows with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first rest =>
    change (do
      let values ← arguments.run (first :: rest)
      ValuesAt.collect (first :: rest) fun r h => body.eval (fun p => .ok (values r h p))) =
        .ok output at called
    rw [argumentsRead] at called
    exact ValuesAt.collect_at called row member

theorem ScopedCalculation.call_argument_failure (body : ScalarBody parameters t)
    (arguments : ScopedArguments scope parameters)
    (first : scope.Member) (rest : List scope.Member) {failure : Failure}
    (failed : arguments.run (first :: rest) = .error failure) :
    (call body arguments).run (first :: rest) = .error failure := by
  -- The helper body cannot catch a failure while evaluating an ordinary argument.
  change (do
    let values ← arguments.run (first :: rest)
    ValuesAt.collect (first :: rest) fun row member =>
      body.eval (fun param => .ok (values row member param))) = .error failure
  rw [failed]
  rfl

-- Source-level ordinary arguments are evaluated before choosing positions
-- at which to run the lifted body. Each yields a materialised value.
inductive OrdinaryArguments (scope : EvaluationScope) : List Ty → Type where
  | nil : OrdinaryArguments scope []
  | cons : (Unit → Outcome (LiftArgument t scope.shape)) → OrdinaryArguments scope rest →
      OrdinaryArguments scope (t :: rest)

def OrdinaryArguments.evaluate :
    OrdinaryArguments scope parameters → Outcome (ScopedArguments scope parameters)
  | .nil => .ok .nil
  | .cons head tail => do
    let value ← head ()
    let rest ← tail.evaluate
    pure (.cons (ScopedCalculation.ofCore value.toCore) rest)

def ScalarBody.callOrdinary (body : ScalarBody parameters t)
    (arguments : OrdinaryArguments scope parameters) (positions : List scope.Member) :
    Outcome (ValuesAt positions t.denote) := do
  let values ← arguments.evaluate
  (ScopedCalculation.call body values).run positions

theorem ScalarBody.callOrdinary_argument_failure (body : ScalarBody parameters t)
    (arguments : OrdinaryArguments scope parameters) (positions : List scope.Member)
    {failure : Failure} (failed : arguments.evaluate = .error failure) :
    body.callOrdinary arguments positions = .error failure := by
  unfold callOrdinary
  rw [failed]
  rfl

-- The shape index is the argument's contribution to the call's iteration.
-- An exact collection match contributes a single value, just like an exact cell match.
inductive ArgumentPassing : Ty → Ty → Shape → Type where
  | whole : ArgumentPassing t t .scalar
  | vector : ArgumentPassing (.vector n t) t (.vector n)
  | column : ArgumentPassing (.column d t) t (.column d)

theorem ArgumentPassing.lift_requires_mismatch (passing : ArgumentPassing actual expected shape)
    (lifted : shape ≠ .scalar) : actual ≠ expected := by
  cases passing with
  | whole => exact False.elim (lifted rfl)
  | vector =>
    intro equal
    have sizes := congrArg sizeOf equal
    simp at sizes
  | column =>
    intro equal
    have sizes := congrArg sizeOf equal
    simp at sizes

theorem ArgumentPassing.whole_iff_match (passing : ArgumentPassing actual expected shape) :
    shape = .scalar ↔ actual = expected := by
  constructor
  · intro whole
    cases passing <;> first | rfl | cases whole
  · intro same
    exact Decidable.byContradiction (fun lifted => passing.lift_requires_mismatch lifted same)

def ArgumentPassing.infer (actual expected : Ty) :
    Option (Σ shape, ArgumentPassing actual expected shape) :=
  if exactMatch : actual = expected then
    some ⟨.scalar, exactMatch ▸ ArgumentPassing.whole (t := actual)⟩
  else
    match actual with
    | .vector n element =>
      if same : element = expected then some ⟨.vector n, same ▸ ArgumentPassing.vector⟩ else none
    | .column d element =>
      if same : element = expected then some ⟨.column d, same ▸ ArgumentPassing.column⟩ else none
    | .required _ | .optional _ | .function .. => none

theorem ArgumentPassing.infer_complete (passing : ArgumentPassing actual expected shape) :
    infer actual expected = some ⟨shape, passing⟩ := by
  cases passing with
  | whole => simp [infer]
  | @vector n =>
    have mismatch : Ty.vector n expected ≠ expected :=
      ArgumentPassing.vector.lift_requires_mismatch (by intro impossible; cases impossible)
    simp [infer, mismatch]
  | @column d =>
    have mismatch : Ty.column d expected ≠ expected :=
      ArgumentPassing.column.lift_requires_mismatch (by intro impossible; cases impossible)
    simp [infer, mismatch]

def ArgumentPassing.valueAt (passing : ArgumentPassing actual expected shape) :
    actual.denote → Position shape → expected.denote :=
  match passing with
  | .whole => fun value _ => value
  | .vector => fun values position => values.get position
  | .column => fun values position => values position

def ArgumentPassing.core (passing : ArgumentPassing actual expected shape) :
    actual.denote → Expr shape expected :=
  match passing with
  | .whole => Expr.literal
  | .vector => Expr.vector
  | .column => Expr.column

theorem ArgumentPassing.eval_core (passing : ArgumentPassing actual expected shape)
    (value : actual.denote) (position : Position shape) :
    (passing.core value).eval position = .ok (passing.valueAt value position) := by
  cases passing <;> rfl

theorem ArgumentPassing.valueAt_of_match (passing : ArgumentPassing actual expected shape)
    (same : actual = expected) (value : actual.denote) (position : Position shape) :
    passing.valueAt value position = cast (congrArg Ty.denote same) value := by
  cases passing with
  | whole => rfl
  | vector =>
    exact False.elim (ArgumentPassing.vector.lift_requires_mismatch (by simp) same)
  | column =>
    exact False.elim (ArgumentPassing.column.lift_requires_mismatch (by simp) same)

inductive ArgumentPassings : List Ty → List Ty → List Shape → Type where
  | nil : ArgumentPassings [] [] []
  | cons : ArgumentPassing actual expected shape → ArgumentPassings actuals parameters shapes →
      ArgumentPassings (actual :: actuals) (expected :: parameters) (shape :: shapes)

def ArgumentPassings.infer : (actuals parameters : List Ty) →
    Option (Σ shapes, ArgumentPassings actuals parameters shapes)
  | [], [] => some ⟨[], .nil⟩
  | actual :: actuals, expected :: parameters => do
    let ⟨shape, head⟩ ← ArgumentPassing.infer actual expected
    let ⟨shapes, tail⟩ ← infer actuals parameters
    pure ⟨shape :: shapes, .cons head tail⟩
  | _, _ => none

theorem ArgumentPassings.infer_complete (passing : ArgumentPassings actuals parameters shapes) :
    infer actuals parameters = some ⟨shapes, passing⟩ := by
  induction passing with
  | nil => rfl
  | cons head tail tailComplete =>
    simp [infer, head.infer_complete, tailComplete, bind, Option.bind, pure]

structure CheckedCall (actuals parameters : List Ty) (result : Shape) where
  shapes : List Shape
  passing : ArgumentPassings actuals parameters shapes
  alignment : AlignArguments shapes result

def CheckedCall.infer (actuals parameters : List Ty) :
    Option (Σ result, CheckedCall actuals parameters result) := do
  let ⟨shapes, passing⟩ ← ArgumentPassings.infer actuals parameters
  let ⟨result, alignment⟩ ← AlignArguments.infer shapes
  pure ⟨result, ⟨shapes, passing, alignment⟩⟩

theorem CheckedCall.infer_complete (call : CheckedCall actuals parameters result) :
    infer actuals parameters = some ⟨result, call⟩ := by
  cases call with
  | mk shapes passing alignment =>
    simp [infer, passing.infer_complete, alignment.infer_complete, bind, Option.bind, pure]

theorem CheckedCall.infer_rejects_iff (actuals parameters : List Ty) :
    infer actuals parameters = none ↔ ¬ Nonempty (Σ result, CheckedCall actuals parameters result) := by
  constructor
  · intro rejected ⟨⟨_, call⟩⟩
    have accepted := call.infer_complete
    rw [rejected] at accepted
    cases accepted
  · intro impossible
    cases infer actuals parameters with
    | none => rfl
    | some call => exact False.elim (impossible ⟨call⟩)

def ArgumentPassings.core (passing : ArgumentPassings actuals parameters shapes)
    (alignment : AlignArguments shapes result) (values : ArgumentValues actuals) :
    CoreArguments parameters result :=
  match passing, alignment with
  | .nil, .nil => fun param => nomatch param
  | .cons head tail, .cons align rest => fun
    | .first => align.leftBroadcast.core (head.core (values .first))
    | .next param => align.rightBroadcast.core (tail.core rest (fun p => values (.next p)) param)

structure ArgumentSlot (actuals : List Ty) (shapes : List Shape) (expected : Ty) where
  actual : Ty
  shape : Shape
  argument : Parameter actuals actual
  contribution : ArgumentShape shapes shape
  passing : ArgumentPassing actual expected shape

def ArgumentPassings.slot (passing : ArgumentPassings actuals parameters shapes) :
    Parameter parameters t → ArgumentSlot actuals shapes t :=
  match passing with
  | .nil => fun param => nomatch param
  | .cons head tail => fun
    | .first => ⟨_, _, .first, .first, head⟩
    | .next param =>
      let slot := tail.slot param
      ⟨slot.actual, slot.shape, .next slot.argument, .next slot.contribution, slot.passing⟩

def ArgumentPassings.valuesAt (passing : ArgumentPassings actuals parameters shapes)
    (alignment : AlignArguments shapes result) (values : ArgumentValues actuals)
    (position : Position result) : ArgumentValues parameters := fun param =>
  let slot := passing.slot param
  slot.passing.valueAt (values slot.argument) (alignment.position position slot.contribution)

theorem ArgumentPassings.eval_core (passing : ArgumentPassings actuals parameters shapes)
    (alignment : AlignArguments shapes result) (values : ArgumentValues actuals)
    (position : Position result) (param : Parameter parameters t) :
    (passing.core alignment values param).eval position =
      .ok (passing.valuesAt alignment values position param) := by
  induction passing generalizing result with
  | nil => nomatch param
  | cons head tail tailCorrect =>
    cases alignment with
    | cons align rest =>
      cases param with
      | first =>
        exact (align.eval_leftBroadcast _ position).trans (head.eval_core _ _)
      | next param =>
        exact (align.eval_rightBroadcast _ position).trans (tailCorrect rest _ _ param)

theorem CheckedCall.parameter_of_match (call : CheckedCall actuals parameters result)
    (param : Parameter parameters t) (values : ArgumentValues actuals)
    (position : Position result) (same : (call.passing.slot param).actual = t) :
    (call.passing.core call.alignment values param).eval position =
      .ok (cast (congrArg Ty.denote same) (values (call.passing.slot param).argument)) := by
  rw [call.passing.eval_core]
  exact congrArg Except.ok ((call.passing.slot param).passing.valueAt_of_match same _ _)

theorem CheckedCall.parameter_whole_iff_match (call : CheckedCall actuals parameters result)
    (param : Parameter parameters t) :
    (call.passing.slot param).shape = .scalar ↔ (call.passing.slot param).actual = t :=
  (call.passing.slot param).passing.whole_iff_match

abbrev ArgumentSources (actuals : List Ty) :=
  ∀ {t}, Parameter actuals t → Unit → Outcome t.denote

def ArgumentSources.evaluate : {actuals : List Ty} → ArgumentSources actuals →
    Outcome (ArgumentValues actuals)
  | [], _ => .ok (fun param => nomatch param)
  | _ :: _, sources => do
    let head ← sources .first ()
    let tail ← evaluate (fun p => sources (.next p))
    pure (ArgumentValues.cons head tail)

def CheckedCall.lower (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals) : Expr result t :=
  body.lower (call.passing.core call.alignment values)

theorem CheckedCall.lower_eq (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals)
    (position : Position result) :
    (call.lower body values).eval position =
      body.eval (fun param => .ok (call.passing.valuesAt call.alignment values position param)) :=
  body.lower_pointwise _ _ position (call.passing.eval_core call.alignment values position)

def CheckedCall.asCalculation (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals) :
    ScopedCalculation scope t.denote :=
  ScopedCalculation.ofCore (call.lower body values)

theorem CheckedCall.asCalculation_eq (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals) :
    call.asCalculation body values = ScopedCalculation.pointwise (fun position =>
      body.eval (fun param => .ok (call.passing.valuesAt call.alignment values position param))) := by
  unfold asCalculation ScopedCalculation.ofCore
  congr 1
  funext position
  exact call.lower_eq body values position

def CheckedCall.runValues (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals)
    (demanded : List (Position result)) : Outcome (List t.denote) :=
  demanded.mapM (call.lower body values).eval

theorem CheckedCall.runValues_eq (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals)
    (demanded : List (Position result)) :
    call.runValues body values demanded = demanded.mapM (fun position =>
      body.eval (fun param => .ok (call.passing.valuesAt call.alignment values position param))) := by
  unfold runValues
  apply congrArg (fun read => demanded.mapM read)
  funext position
  exact call.lower_eq body values position

theorem CheckedCall.asCalculation_toList (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals)
    (demanded : List scope.Member) :
    (ValuesAt.toList <$> (call.asCalculation body values).run demanded) =
      call.runValues body values (demanded.map Subtype.val) := by
  simp only [asCalculation, ScopedCalculation.ofCore, runValues,
    ScopedCalculation.pointwise_toList, List.mapM_map]
  rfl

theorem CheckedCall.transform_lifted (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (values : ArgumentValues actuals)
    (operation : List t.denote → Outcome (scope.Member → α))
    (first : scope.Member) (rest : List scope.Member) :
    ((call.asCalculation body values).transform (fun input => operation input.toList)).run (first :: rest) =
      (do
        let input ← call.runValues body values (scope.members.map Subtype.val)
        let output ← operation input
        pure (fun row _ => output row)) := by
  rw [← call.asCalculation_toList body values scope.members]
  change (do
    let input ← (call.asCalculation body values).run scope.members
    let output ← operation input.toList
    pure (fun row _ => output row)) = _
  cases (call.asCalculation body values).run scope.members <;> rfl

def CheckedCall.run (call : CheckedCall actuals parameters result) (body : ScalarBody parameters t)
    (sources : ArgumentSources actuals) (demanded : List (Position result)) :
    Outcome (List t.denote) := do
  let values ← sources.evaluate
  call.runValues body values demanded

def CheckedCall.runScoped (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals)
    (demanded : List scope.Member) : Outcome (List t.denote) := do
  let values ← sources.evaluate
  ValuesAt.toList <$> (call.asCalculation body values).run demanded

theorem CheckedCall.runScoped_eq (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals)
    (demanded : List scope.Member) :
    call.runScoped body sources demanded = call.run body sources (demanded.map Subtype.val) := by
  simp only [runScoped, run, asCalculation_toList]

-- The enclosing guard can skip the entire call. Once demanded, ordinary
-- arguments materialize before any position of the lifted body runs.
def CheckedCall.scopedCall (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals) :
    ScopedCalculation scope t.denote :=
  ⟨fun first rest => do
    let values ← sources.evaluate
    (call.asCalculation body values).run (first :: rest)⟩

theorem CheckedCall.scopedCall_toList (call : CheckedCall actuals parameters scope.shape)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals)
    (demanded : List scope.Member) :
    (ValuesAt.toList <$> (call.scopedCall body sources).run demanded) =
      match demanded with
      | [] => .ok []
      | _ :: _ => call.run body sources (demanded.map Subtype.val) := by
  cases demanded with
  | nil => rfl
  | cons first rest =>
    rw [← call.runScoped_eq]
    cases evaluated : sources.evaluate <;>
      simp [ScopedCalculation.run, scopedCall, runScoped, evaluated, bind, Except.bind] <;> rfl

theorem CheckedCall.scopedCall_transport (call : CheckedCall actuals parameters result)
    (compatible : result = scope.shape) (body : ScalarBody parameters t)
    (sources : ArgumentSources actuals) (demanded : List scope.Member) :
    let alignedCall : CheckedCall actuals parameters scope.shape := compatible ▸ call
    (ValuesAt.toList <$> (alignedCall.scopedCall body sources).run demanded) =
      match demanded with
      | [] => .ok []
      | _ :: _ => call.run body sources
        (demanded.map fun row => cast (congrArg Position compatible.symm) row.val) := by
  subst result
  exact call.scopedCall_toList body sources demanded

-- Both executions materialise arguments in source order, even with no demand.
-- The scalar reference reads whole values or aligned elements at each demanded position.
theorem CheckedCall.run_eq (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals)
    (demanded : List (Position result)) :
    call.run body sources demanded = (do
      let values ← sources.evaluate
      demanded.mapM fun position =>
        body.eval (fun param => .ok (call.passing.valuesAt call.alignment values position param))) := by
  simp only [run, runValues_eq]

theorem CheckedCall.run_argument_failure (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals)
    (demanded : List (Position result)) {failure : Failure}
    (failed : sources.evaluate = .error failure) :
    call.run body sources demanded = .error failure := by
  rw [run, failed]
  rfl

private theorem firstFailure_mapM (read : α → Outcome β) (demanded : List α)
    (failure : Failure) :
    demanded.mapM read = .error failure ↔
      ∃ earlier position later, demanded = earlier ++ position :: later ∧
        (∀ p ∈ earlier, ∃ value, read p = .ok value) ∧ read position = .error failure := by
  induction demanded with
  | nil => simp [pure, Except.pure]
  | cons first rest tailCorrect =>
    constructor
    · intro failed
      rw [List.mapM_cons] at failed
      cases firstRead : read first with
      | error error =>
        rw [firstRead] at failed
        cases failed
        exact ⟨[], first, rest, rfl, by simp, firstRead⟩
      | ok value =>
        rw [firstRead] at failed
        cases tailRead : rest.mapM read with
        | ok _ =>
          simp [bind, Except.bind, tailRead, pure, Except.pure] at failed
        | error _ =>
          simp only [bind, Except.bind, tailRead] at failed
          cases failed
          obtain ⟨earlier, position, later, rfl, allOk, positionFailed⟩ :=
            tailCorrect.mp tailRead
          refine ⟨first :: earlier, position, later, rfl, ?_, positionFailed⟩
          intro p member
          rcases List.mem_cons.mp member with rfl | inTail
          · exact ⟨value, firstRead⟩
          · exact allOk p inTail
    · rintro ⟨earlier, position, later, split, allOk, positionFailed⟩
      cases earlier with
      | nil =>
        cases split
        simp [List.mapM_cons, positionFailed, bind, Except.bind]
      | cons head earlier =>
        cases split
        obtain ⟨value, firstRead⟩ := allOk _ List.mem_cons_self
        have remainingFailed := tailCorrect.mpr
          ⟨earlier, position, later, rfl,
            fun p member => allOk p (List.mem_cons_of_mem _ member), positionFailed⟩
        rw [List.mapM_cons, firstRead, remainingFailed]
        rfl

theorem CheckedCall.run_demanded_failure_iff (call : CheckedCall actuals parameters result)
    (body : ScalarBody parameters t) (sources : ArgumentSources actuals)
    (demanded : List (Position result)) {values : ArgumentValues actuals}
    (evaluated : sources.evaluate = .ok values) (failure : Failure) :
    let read := fun position =>
      body.eval (fun param => .ok (call.passing.valuesAt call.alignment values position param))
    call.run body sources demanded = .error failure ↔
      ∃ earlier position later, demanded = earlier ++ position :: later ∧
        (∀ p ∈ earlier, ∃ value, read p = .ok value) ∧ read position = .error failure := by
  rw [call.run_eq, evaluated]
  exact firstFailure_mapM _ demanded failure

end Rupam
