import Rupam.Core

namespace Rupam

-- Parameters are resolved binding positions, indexed by their checked type.
inductive Parameter : List Ty → Ty → Type where
  | first : Parameter (t :: rest) t
  | next : Parameter rest t → Parameter (other :: rest) t

-- A checked, purely pointwise helper body. Promotion and presence coercions
-- have already been resolved. Group operations belong to the grouped calculus.
inductive ScalarBody (parameters : List Ty) : Ty → Type where
  | arg : Parameter parameters t → ScalarBody parameters t
  | literal : t.denote → ScalarBody parameters t
  | optional : ScalarBody parameters (.required b) → ScalarBody parameters (.optional b)
  | add : ScalarBody parameters (.required .int) → ScalarBody parameters (.required .int) →
      ScalarBody parameters (.required .int)
  | multiply : ScalarBody parameters (.required .int) → ScalarBody parameters (.required .int) →
      ScalarBody parameters (.required .int)
  | addOptional : ScalarBody parameters (.optional .int) → ScalarBody parameters (.optional .int) →
      ScalarBody parameters (.optional .int)
  | multiplyOptional : ScalarBody parameters (.optional .int) → ScalarBody parameters (.optional .int) →
      ScalarBody parameters (.optional .int)
  | apply : ScalarBody parameters (.function input output) → ScalarBody parameters input →
      ScalarBody parameters output
  | ifThenElse : ScalarBody parameters (.required .bool) → ScalarBody parameters t →
      ScalarBody parameters t → ScalarBody parameters t
  | coalesce : ScalarBody parameters (.optional b) → ScalarBody parameters (.required b) →
      ScalarBody parameters (.required b)
  | coalesceOptional : ScalarBody parameters (.optional b) → ScalarBody parameters (.optional b) →
      ScalarBody parameters (.optional b)
  | and : ScalarBody parameters (.optional .bool) → ScalarBody parameters (.optional .bool) →
      ScalarBody parameters (.optional .bool)
  | or : ScalarBody parameters (.optional .bool) → ScalarBody parameters (.optional .bool) →
      ScalarBody parameters (.optional .bool)
  | not : ScalarBody parameters (.optional .bool) → ScalarBody parameters (.optional .bool)

abbrev CoreArguments (parameters : List Ty) (shape : Shape) :=
  ∀ {t}, Parameter parameters t → Expr shape t

abbrev ScalarArguments (parameters : List Ty) :=
  ∀ {t}, Parameter parameters t → Outcome t.denote

def ScalarBody.eval (arguments : ScalarArguments parameters) :
    ScalarBody parameters t → Outcome t.denote
  | .arg param => arguments param
  | .literal value => .ok value
  | .optional value => some <$> value.eval arguments
  | .add left right => do addInt (← left.eval arguments) (← right.eval arguments)
  | .multiply left right => do multiplyInt (← left.eval arguments) (← right.eval arguments)
  | .addOptional left right => do lift₂ addInt (← left.eval arguments) (← right.eval arguments)
  | .multiplyOptional left right => do
    lift₂ multiplyInt (← left.eval arguments) (← right.eval arguments)
  | .apply function argument => do
    let invoke ← function.eval arguments
    invoke (← argument.eval arguments)
  | .ifThenElse condition yes no => do
    choose (← condition.eval arguments) (fun _ => yes.eval arguments) (fun _ => no.eval arguments)
  | .coalesce left right => Rupam.coalesce (left.eval arguments) (fun _ => right.eval arguments)
  | .coalesceOptional left right =>
    coalesceWith (left.eval arguments) (fun _ => right.eval arguments) some
  | .and left right => do and3 (← left.eval arguments) (fun _ => right.eval arguments)
  | .or left right => do or3 (← left.eval arguments) (fun _ => right.eval arguments)
  | .not value => not3 <$> value.eval arguments

def ScalarBody.lower (arguments : CoreArguments parameters shape) :
    ScalarBody parameters t → Expr shape t
  | .arg param => arguments param
  | .literal value => (Expr.literal value).broadcastTo shape
  | .optional value => .optional (value.lower arguments)
  | .add left right => .add (Align.pointwise shape) (left.lower arguments) (right.lower arguments)
  | .multiply left right =>
    .multiply (Align.pointwise shape) (left.lower arguments) (right.lower arguments)
  | .addOptional left right =>
    .addOptional (Align.pointwise shape) (left.lower arguments) (right.lower arguments)
  | .multiplyOptional left right =>
    .multiplyOptional (Align.pointwise shape) (left.lower arguments) (right.lower arguments)
  | .apply function argument => .apply (function.lower arguments) (argument.lower arguments)
  | .ifThenElse condition yes no =>
    .ifThenElse (condition.lower arguments) (yes.lower arguments) (no.lower arguments)
  | .coalesce left right => .coalesce (left.lower arguments) (right.lower arguments)
  | .coalesceOptional left right => .coalesceOptional (left.lower arguments) (right.lower arguments)
  | .and left right => .and (left.lower arguments) (right.lower arguments)
  | .or left right => .or (left.lower arguments) (right.lower arguments)
  | .not value => .not (value.lower arguments)

theorem ScalarBody.lower_pointwise (body : ScalarBody parameters t)
    (shaped : CoreArguments parameters shape) (scalar : ScalarArguments parameters)
    (position : Position shape)
    (agree : ∀ {a} (param : Parameter parameters a),
      (shaped param).eval position = scalar param) :
    (body.lower shaped).eval position = body.eval scalar := by
  induction body with
  | arg param =>
    exact agree param
  | literal value =>
    exact Expr.eval_broadcastTo (.literal value) position
  | optional value sameValue =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameValue]
  | add left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, Align.pointwise_positions,
      sameLeft, sameRight]
  | multiply left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, Align.pointwise_positions,
      sameLeft, sameRight]
  | addOptional left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, Align.pointwise_positions,
      sameLeft, sameRight]
  | multiplyOptional left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, Align.pointwise_positions,
      sameLeft, sameRight]
  | apply function argument sameFunction sameArgument =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameFunction, sameArgument]
  | ifThenElse condition yes no sameCondition sameYes sameNo =>
    -- Agreement in the condition selects the same thunk on both sides.
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameCondition, sameYes, sameNo]
  | coalesce left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameLeft, sameRight]
  | coalesceOptional left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameLeft, sameRight]
  | and left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameLeft, sameRight]
  | or left right sameLeft sameRight =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameLeft, sameRight]
  | not value sameValue =>
    simp only [ScalarBody.lower, ScalarBody.eval, Expr.eval, sameValue]

-- A substituted pointwise argument can itself be the body of another helper.
theorem ScalarBody.lower_nested_pointwise (body : ScalarBody parameters t)
    (inner : ∀ {a}, Parameter parameters a → ScalarBody outer a)
    (shaped : CoreArguments outer shape) (scalar : ScalarArguments outer)
    (position : Position shape)
    (agree : ∀ {a} (param : Parameter outer a),
      (shaped param).eval position = scalar param) :
    (body.lower (fun param => (inner param).lower shaped)).eval position =
      body.eval (fun param => (inner param).eval scalar) := by
  apply body.lower_pointwise
  intro _ param
  exact (inner param).lower_pointwise shaped scalar position agree

-- Ordinary arguments are values. A shaped argument must already have the
-- consuming shape, while an ordinary scalar is broadcast explicitly.
inductive LiftArgument (t : Ty) : Shape → Type where
  | scalar : t.denote → LiftArgument t shape
  | vector : Vector t.denote n → LiftArgument t (.vector n)
  | column : (Row d → t.denote) → LiftArgument t (.column d)

def LiftArgument.valueAt : LiftArgument t shape → Position shape → t.denote
  | .scalar value, _ => value
  | .vector cells, index => cells.get index
  | .column cells, row => cells row

def LiftArgument.toCore : LiftArgument t shape → Expr shape t
  | .scalar value => (Expr.literal value).broadcastTo shape
  | .vector cells => .vector cells
  | .column cells => .column cells

theorem LiftArgument.eval_toCore (argument : LiftArgument t shape) (position : Position shape) :
    argument.toCore.eval position = .ok (argument.valueAt position) := by
  cases argument with
  | scalar value => exact Expr.eval_broadcastTo (.literal value) position
  | vector cells => rfl
  | column cells => rfl

theorem ScalarBody.lifting (body : ScalarBody parameters t)
    (arguments : ∀ {a}, Parameter parameters a → LiftArgument a shape)
    (position : Position shape) :
    (body.lower (fun param => (arguments param).toCore)).eval position =
      body.eval (fun param => .ok ((arguments param).valueAt position)) := by
  -- Each input agrees at this position, so the whole body agrees, including
  -- missing values, failures, and the branches it needs to evaluate.
  apply body.lower_pointwise
  intro _ param
  exact (arguments param).eval_toCore position

def Expr.collectVector (expression : Expr (.vector size) t) : Outcome (List t.denote) :=
  (List.finRange size).mapM expression.eval

theorem ScalarBody.vector_lifting (body : ScalarBody parameters t)
    (arguments : ∀ {a}, Parameter parameters a → LiftArgument a (.vector size)) :
    (body.lower (fun param => (arguments param).toCore)).collectVector =
      (List.finRange size).mapM
        (fun position => body.eval (fun param => .ok ((arguments param).valueAt position))) := by
  -- Whole-vector consumers see the scalar results in vector position order.
  unfold Expr.collectVector
  congr 1
  funext position
  exact body.lifting arguments position

end Rupam
