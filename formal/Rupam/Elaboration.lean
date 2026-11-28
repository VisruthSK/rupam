import Rupam.Deferred
import Rupam.Alignment

namespace Rupam

inductive SourceName where
  | bare (name : String)
  | lexical (name : String)
  | column (name : String)

inductive SourceExpr where
  | integer (value : Int)
  | integerVector (values : List Int)
  | boolean (value : Bool)
  | missing (annotation : Base)
  | reference (name : SourceName)
  | add (left right : SourceExpr)
  | conditional (condition yes no : SourceExpr)

structure CheckedValue where
  shape : Shape
  type : Ty
  core : Expr shape type

structure SourceContext where
  lexical : String → Option CheckedValue
  stage : Option Stage := none

inductive ElaborationError where
  | integerRange
  | unknownName (name : SourceName)
  | ambiguousName (name : String)
  | misaligned (left right : Shape)
  | expectedNumbers (left right : Ty)
  | optionalOrNonBooleanCondition (actual : Ty)
  | incompatibleBranches (left right : Ty)
  | incompatibleConditionalShapes (condition yes no : Shape)

private def requireOption (value : Option α) (error : ElaborationError) :
    Except ElaborationError α :=
  match value with
  | some found => .ok found
  | none => .error error

private def checkedIntegers :
    (values : List Int) → Except ElaborationError {cells : List RInt // cells.map Subtype.val = values}
  | [] => .ok ⟨[], rfl⟩
  | value :: rest =>
    if portable : Portable value then do
      let cells ← checkedIntegers rest
      pure ⟨⟨value, portable⟩ :: cells.val, congrArg (List.cons value) cells.property⟩
    else .error .integerRange

private theorem checkedIntegers_agrees (cells : List RInt) :
    checkedIntegers (cells.map Subtype.val) = .ok ⟨cells, rfl⟩ := by
  induction cells with
  | nil => rfl
  | cons cell rest agrees =>
    change checkedIntegers (cell.val :: rest.map Subtype.val) = .ok ⟨cell :: rest, rfl⟩
    rw [checkedIntegers, dite_eq_left cell.property, agrees]
    rfl

def Stage.lookup (stage : Stage) (name : String) : Option CheckedValue :=
  match found : stage.schema name with
  | none => none
  | some t => some ⟨.column stage.domain, t, .column (stage.read name t found)⟩

def SourceContext.resolve (context : SourceContext) : SourceName → Except ElaborationError CheckedValue
  | .lexical name => requireOption (context.lexical name) (.unknownName (.lexical name))
  | .column name =>
    requireOption (context.stage.bind (·.lookup name)) (.unknownName (.column name))
  | .bare name =>
    match context.lexical name, context.stage.bind (·.lookup name) with
    | some _, some _ => .error (.ambiguousName name)
    | some binding, none | none, some binding => .ok binding
    | none, none => .error (.unknownName (.bare name))

inductive IntOperands : Ty → Ty → Ty → Type where
  | required : IntOperands (.required .int) (.required .int) (.required .int)
  | optionalLeft : IntOperands (.optional .int) (.required .int) (.optional .int)
  | optionalRight : IntOperands (.required .int) (.optional .int) (.optional .int)
  | optionalBoth : IntOperands (.optional .int) (.optional .int) (.optional .int)

def IntOperands.infer : (a b : Ty) → Option (Σ t, IntOperands a b t)
  | .required .int, .required .int => some ⟨_, .required⟩
  | .optional .int, .required .int => some ⟨_, .optionalLeft⟩
  | .required .int, .optional .int => some ⟨_, .optionalRight⟩
  | .optional .int, .optional .int => some ⟨_, .optionalBoth⟩
  | _, _ => none

theorem IntOperands.infer_complete (operands : IntOperands a b t) :
    infer a b = some ⟨t, operands⟩ := by
  cases operands <;> rfl

def IntOperands.lower (alignment : Align sa sb result) :
    IntOperands a b t → Expr sa a → Expr sb b → Expr result t
  | .required, left, right => .add alignment left right
  | .optionalLeft, left, right => .addOptional alignment left (.optional right)
  | .optionalRight, left, right => .addOptional alignment (.optional left) right
  | .optionalBoth, left, right => .addOptional alignment left right

def IntOperands.compute : IntOperands a b t → a.denote → b.denote → Outcome t.denote
  | .required, left, right => addInt left right
  | .optionalLeft, left, right => lift₂ addInt left (some right)
  | .optionalRight, left, right => lift₂ addInt (some left) right
  | .optionalBoth, left, right => lift₂ addInt left right

theorem IntOperands.lower_observes (operands : IntOperands a b t)
    (alignment : Align sa sb shape) (left : Expr sa a) (right : Expr sb b)
    (position : Position shape) :
    (operands.lower alignment left right).eval position = (do
      let (l, r) := alignment.positions position
      operands.compute (← left.eval l) (← right.eval r)) := by
  cases operands <;> simp only [lower, Expr.eval, compute]
  all_goals
    cases left.eval (alignment.positions position).1 <;>
      cases right.eval (alignment.positions position).2 <;> rfl

inductive CommonType : Ty → Ty → Ty → Type where
  | same : CommonType t t t
  | leftOptional : CommonType (.optional b) (.required b) (.optional b)
  | rightOptional : CommonType (.required b) (.optional b) (.optional b)

def CommonType.infer : (a b : Ty) → Option (Σ t, CommonType a b t)
  | .optional a, .required b =>
    if same : a = b then by subst b; exact some ⟨_, .leftOptional⟩ else none
  | .required a, .optional b =>
    if same : a = b then by subst b; exact some ⟨_, .rightOptional⟩ else none
  | a, b =>
    if same : a = b then by subst b; exact some ⟨_, .same⟩ else none

theorem CommonType.infer_complete (common : CommonType a b t) :
    infer a b = some ⟨t, common⟩ := by
  cases common with
  | same => cases a <;> simp [infer]
  | leftOptional => simp [infer]
  | rightOptional => simp [infer]

def CommonType.left : CommonType a b t → Expr shape a → Expr shape t
  | .same, value | .leftOptional, value => value
  | .rightOptional, value => .optional value

def CommonType.right : CommonType a b t → Expr shape b → Expr shape t
  | .same, value | .rightOptional, value => value
  | .leftOptional, value => .optional value

def CommonType.leftValue : CommonType a b t → a.denote → t.denote
  | .same, value | .leftOptional, value => value
  | .rightOptional, value => some value

def CommonType.rightValue : CommonType a b t → b.denote → t.denote
  | .same, value | .rightOptional, value => value
  | .leftOptional, value => some value

-- The source judgment is a syntax-directed derivation. Shape and value type
-- determine the Core type, and each accepted reference records its resolution.
inductive Elaborates (context : SourceContext) : SourceExpr → Shape → Ty → Type where
  | integer (value : Int) (portable : Portable value) :
      Elaborates context (.integer value) .scalar (.required .int)
  | integerVector (cells : List RInt) :
      Elaborates context (.integerVector (cells.map Subtype.val)) (.vector cells.length) (.required .int)
  | boolean (value : Bool) : Elaborates context (.boolean value) .scalar (.required .bool)
  | missing (annotation : Base) : Elaborates context (.missing annotation) .scalar (.optional annotation)
  | reference (value : CheckedValue) (found : context.resolve name = .ok value) :
      Elaborates context (.reference name) value.shape value.type
  | add : Elaborates context left ls lt → Elaborates context right rs rt →
      Align ls rs shape → IntOperands lt rt t → Elaborates context (.add left right) shape t
  | conditional : Elaborates context condition cs (.required .bool) →
      Elaborates context yes ys yt → Elaborates context no ns nt →
      AlignArguments [cs, ys, ns] shape → CommonType yt nt t →
      Elaborates context (.conditional condition yes no) shape t

def Elaborates.core : Elaborates context source shape t → Expr shape t
  | .integer value portable => .literal ⟨value, portable⟩
  | .integerVector cells => .vector (Vector.ofFn (fun index => cells[index.val]'(index.isLt)))
  | .boolean value => .literal value
  | .missing _ => .literal none
  | .reference value _ => value.core
  | .add left right alignment operands => operands.lower alignment left.core right.core
  | .conditional condition yes no alignment common =>
    .ifThenElse ((alignment.broadcasts .first).core condition.core)
      (common.left ((alignment.broadcasts (.next .first)).core yes.core))
      (common.right ((alignment.broadcasts (.next (.next .first))).core no.core))

structure Inferred (context : SourceContext) (source : SourceExpr) where
  shape : Shape
  type : Ty
  derivation : Elaborates context source shape type

def SourceExpr.infer (context : SourceContext) :
    (source : SourceExpr) → Except ElaborationError (Inferred context source)
  | .integer value =>
    if portable : Portable value then .ok ⟨_, _, .integer value portable⟩
    else .error .integerRange
  | .integerVector values => do
    let cells ← checkedIntegers values
    pure ⟨.vector cells.val.length, .required .int,
      congrArg SourceExpr.integerVector cells.property ▸ .integerVector cells.val⟩
  | .boolean value => .ok ⟨_, _, .boolean value⟩
  | .missing annotation => .ok ⟨_, _, .missing annotation⟩
  | .reference name =>
    match found : context.resolve name with
    | .ok value => .ok ⟨_, _, .reference value found⟩
    | .error diagnostic => .error diagnostic
  | .add left right => do
    let a ← left.infer context
    let b ← right.infer context
    let ⟨shape, align⟩ ← requireOption (Align.infer a.shape b.shape) (.misaligned a.shape b.shape)
    let ⟨t, operands⟩ ← requireOption (IntOperands.infer a.type b.type) (.expectedNumbers a.type b.type)
    pure ⟨shape, t, .add a.derivation b.derivation align operands⟩
  | .conditional condition yes no => do
    let test ← condition.infer context
    if requiredBool : test.type = .required .bool then
      let a ← yes.infer context
      let b ← no.infer context
      let ⟨shape, align⟩ ← requireOption (AlignArguments.infer [test.shape, a.shape, b.shape])
        (.incompatibleConditionalShapes test.shape a.shape b.shape)
      let ⟨t, common⟩ ← requireOption (CommonType.infer a.type b.type)
        (.incompatibleBranches a.type b.type)
      pure ⟨shape, t, .conditional (requiredBool ▸ test.derivation) a.derivation b.derivation align common⟩
    else
      .error (.optionalOrNonBooleanCondition test.type)

theorem Elaborates.infer_agrees (derivation : Elaborates context source shape t) :
    source.infer context = .ok ⟨shape, t, derivation⟩ := by
  induction derivation with
  | integer value portable =>
    simp [SourceExpr.infer, portable]
  | integerVector cells =>
    rw [SourceExpr.infer, checkedIntegers_agrees]
    rfl
  | boolean value => rfl
  | missing annotation => rfl
  | reference value found =>
    unfold SourceExpr.infer
    split
    · rename_i resolved success
      have same : resolved = value := Except.ok.inj (success.symm.trans found)
      subst resolved
      rfl
    · rename_i diagnostic failed
      rw [failed] at found
      cases found
  | add left right alignment operands leftAgrees rightAgrees =>
    rw [SourceExpr.infer, leftAgrees, rightAgrees]
    simp [bind, Except.bind, requireOption, alignment.infer_complete, operands.infer_complete]
    rfl
  | conditional condition yes no alignment common conditionAgrees yesAgrees noAgrees =>
    rw [SourceExpr.infer, conditionAgrees]
    simp [bind, Except.bind, requireOption, yesAgrees, noAgrees,
      alignment.infer_complete, common.infer_complete]
    rfl

theorem Elaborates.coherent (first : Elaborates context source shape₁ t₁)
    (second : Elaborates context source shape₂ t₂) :
    (⟨shape₁, t₁, first⟩ : Inferred context source) = ⟨shape₂, t₂, second⟩ := by
  exact Except.ok.inj (first.infer_agrees.symm.trans second.infer_agrees)

theorem Elaborates.vector_add_at_position
    (left : Elaborates context a (.vector n) ta) (right : Elaborates context b (.vector n) tb)
    (operands : IntOperands ta tb t) (index : Fin n) :
    (Elaborates.add left right .vectorSame operands).core.eval index = (do
      operands.compute (← left.core.eval index) (← right.core.eval index)) :=
  operands.lower_observes .vectorSame left.core right.core index

end Rupam
