import Rupam
import Lean.Data.Json
import Lean.Elab.Command
import Lean.Util.CollectAxioms

open Rupam

private def integer (n : Int) (h : Portable n := by decide) : RInt := ⟨n, h⟩

private def boolLit (b : Option Bool) : Expr .scalar (.optional .bool) := .literal b

private def renderOption (render : α → String) : Option α → String
  | some value => render value
  | none => "None"

private def tuple (cells : List String) : String :=
  "(" ++ String.intercalate ", " cells ++ ")"

private def c52 : Outcome String := do
  let missing := boolLit none
  let falsity := boolLit (some false)
  let truth := boolLit (some true)
  let exprs := [Expr.and falsity missing, .or truth missing, .and truth missing,
    .or falsity missing, .not missing]
  let values ← exprs.mapM (·.eval ())
  pure (tuple (values.map (renderOption (fun b => if b then "TRUE" else "FALSE"))))

private def c56 : Outcome String := do
  let inputs := [[], [none, none], [some (integer 0), some (integer 0)]]
  let values ← inputs.mapM (sumInt .skip)
  pure (tuple (values.map (renderOption (fun n => toString n.val))))

-- These fixtures use exactly representable inputs and means.
private def fixtureMean (values : List Float) : Outcome (Option Float) :=
  if values.isEmpty then .ok none
  else .ok (some (values.sum / values.length.toFloat))

private def renderFloatColumn (header : String) (cells : List (Option Float)) : String :=
  let render (value : Float) :=
    let text := toString value
    if text.endsWith ".000000" then (text.dropEnd 5).toString else text
  String.intercalate "\n" (header :: cells.map (renderOption render))

private def c119 : Outcome String := do
  let group : Group ⟨0, 2⟩ := ⟨[0, 1], by decide⟩
  let scope := EvaluationScope.group group
  let x : Vector Float 2 := #v[2.0, 4.0]
  let flags : ScopedCalculation scope Bool := .pointwise (fun row => .ok (row.val == 0))
  let values : ScopedCalculation scope Float := .pointwise (fun row => .ok (x.get row))
  let zero : ScopedCalculation scope (Option Float) := .broadcast (fun _ => .ok (some 0.0))
  let result ← (flags.ifThenElse (values.reduce fixtureMean) zero).run scope.members
  pure (renderFloatColumn "r" result.toList)

private def c123 : Outcome String := do
  let x : Vector (Option Float) 3 := #v[none, some 5.0, some 2.0]
  let y : Vector Float 3 := #v[1.0, 3.0, 7.0]
  let groupA : Group ⟨1, 3⟩ := ⟨[0, 1], by decide⟩
  let groupB : Group ⟨1, 3⟩ := ⟨[2], by decide⟩
  let evaluate (group : Group ⟨1, 3⟩) : Outcome (List (Option Float)) := do
    let scope := EvaluationScope.group group
    let left : ScopedCalculation scope (Option Float) :=
      .pointwise (fun row => .ok (x.get row))
    let values : ScopedCalculation scope Float := .pointwise (fun row => .ok (y.get row))
    let cells ← (left.coalesce (values.reduce fixtureMean) some).run scope.members
    pure cells.toList
  let a ← evaluate groupA
  let b ← evaluate groupB
  pure (renderFloatColumn "r" (a ++ b))

private def floatType : ContextType Unit := .calculation ()
private def centerNeeds := [Requirement.collection .group, .missing (.fixed .skip)]

private def centerHelper :
    ContextTerm (fun _ : Unit => Float) [floatType] (.function centerNeeds floatType floatType) [] :=
  .lambda (.combine (fun value mean => .ok (value - mean))
    (.variable .first)
    (.apply (.reducer (output := ()) .group (.fixed .skip) fun _ values =>
      if values.isEmpty then .error .missingValue else .ok (values.sum / values.length.toFloat))
      (.variable .first)))

private def c124 : Outcome String := do
  let cells : Vector Float 3 := #v[1.0, 3.0, 5.0]
  let groupA : Group ⟨2, 3⟩ := ⟨[0, 1], by decide⟩
  let groupB : Group ⟨2, 3⟩ := ⟨[2], by decide⟩
  let applyCenter : ContextTerm (fun _ : Unit => Float) [floatType] floatType centerNeeds :=
    .apply (.apply (.forward centerNeeds floatType floatType) centerHelper) (.variable .first)
  let evaluate (group : Group ⟨2, 3⟩) : Outcome (List Float) := do
    let context : ConsumptionContext := { scope := .group group }
    let environment : ContextEnvironment (fun _ : Unit => Float) context [floatType]
      | _, .first => .pointwise (fun row => .ok (cells.get row))
    match applyCenter.run context environment with
    | .error _ => .error .missingValue
    | .ok calculation => ValuesAt.toList <$> calculation.run context.scope.members
  let a ← evaluate groupA
  let b ← evaluate groupB
  pure (renderFloatColumn "z" ((a ++ b).map some))

-- C110 exercises both paths with the same overflowing group.
private def group : Group ⟨0, 2⟩ := ⟨[0, 1], by decide⟩

private def groupSum (rows : List (Row ⟨0, 2⟩)) : Outcome (Option RInt) :=
  sumInt .skip (rows.map fun row => some (if row.val = 0 then integer intMax else integer 1))

example : guardedReduction group (fun _ => false) groupSum = .ok [] := rfl

example : guardedReduction group (fun row => row.val = 0) groupSum =
    .error .arithmeticOverflow := rfl

-- C111 checks the final exact sum, even when a prefix exceeds the Int range.
example : sumInt .skip [some (integer intMax), some (integer 1), some (integer (-intMax))] =
    .ok (some (integer 1)) := rfl

example : sumInt .keep [some (integer 2), none] = .ok none := rfl
example : sumInt .error [some (integer 2), none] = .error .missingValue := rfl

-- C100's two helpers share a caller but differ in lexical capture.
private def caller : PolicyContext := { verb := some .keep }

example : (PolicyDemand.capture none (some .skip)).resolve caller = .ok .skip := rfl
example : (PolicyDemand.capture none none).resolve caller = .ok .keep := rfl

-- A missing left cell on an inactive row cannot demand the fallback.
private def leftCell (row : Row ⟨0, 2⟩) : Outcome (Option RInt) :=
  .ok (if row.val = 0 then some (integer 2) else none)

example : group.coalesce (fun row => row.val = 0) leftCell groupSum =
    .ok [(0, some (integer 2))] := rfl

-- Activating the missing cell demands the sum over both rows, which overflows.
example : group.coalesce (fun row => row.val = 1) leftCell groupSum =
    .error .arithmeticOverflow := rfl

-- The failing row is inactive in the lifted choice and the fallback succeeds.
private def guardedAdd : Expr (.column ⟨0, 2⟩) (.required .int) :=
  .ifThenElse (.column fun row => row.val = 1)
    (.add .right (.column fun row => if row.val = 0 then integer intMax else integer 2)
      (.literal (integer 1)))
    (.broadcast (.literal (integer 0)))

example : guardedAdd.eval 0 = .ok (integer 0) := rfl
example : guardedAdd.eval 1 = .ok (integer 3) := rfl

private def plus : ScalarBody [.required .int, .required .int] (.required .int) :=
  .add (.arg .first) (.arg (.next .first))

private def vectorArguments {t : Ty} :
    Parameter [.required .int, .required .int] t → LiftArgument t (.vector 2)
  | .first => .vector #v[integer 2, integer intMax]
  | .next .first => .scalar (integer 1)

private def vectorAdd := plus.lower (fun param => (vectorArguments param).toCore)

example : vectorAdd.eval 0 = .ok (integer 3) := rfl
example : vectorAdd.eval 1 = .error .arithmeticOverflow := rfl

example : Align.infer (.vector 1) (.vector 2) = none := rfl
example : Align.infer (.vector 2) (.column ⟨0, 2⟩) = none := rfl
example : Align.infer (.column ⟨0, 2⟩) (.column ⟨1, 2⟩) = none := rfl
example : AlignArguments.infer [.vector 2, .scalar, .vector 3] = none := rfl
example : AlignArguments.infer [.column ⟨0, 2⟩, .scalar, .column ⟨1, 2⟩] = none := rfl

private def groupScope := EvaluationScope.group group

private def groupedFallback : ScopedCalculation groupScope (Option RInt) :=
  let source : ScopedCalculation groupScope (Row ⟨0, 2⟩) := .pointwise (.ok ·)
  (ScopedCalculation.pointwise leftCell).coalesce (source.reduce groupSum) some

example : ValuesAt.toList <$> groupedFallback.run (groupScope.members.filter (·.val.val == 0)) =
    .ok [some (integer 2)] := rfl

example : ValuesAt.toList <$> groupedFallback.run (groupScope.members.filter (·.val.val == 1)) =
    .error .arithmeticOverflow := rfl

private def vectorScope := EvaluationScope.vector 2

private def vectorValues : ScopedCalculation vectorScope (Option RInt) :=
  ScopedCalculation.ofCore (t := .optional .int) (.vector #v[some (integer 3), some (integer 4)])

private def vectorReduction : ScopedCalculation vectorScope (Option RInt) :=
  vectorValues.reduce (sumInt .skip)

example : ValuesAt.toList <$>
    vectorReduction.run (vectorScope.members.filter (·.val.val == 1)) =
    .ok [some (integer 7)] := rfl

example :
    let empty : ScopedCalculation (.vector 0) (Option RInt) := .pointwise (fun _ => .ok none)
    empty.reduceWhole (sumInt .skip) = .ok none := rfl

example :
    let shifted := vectorValues.zipWith vectorReduction (lift₂ addInt)
    shifted.reduceWhole (sumInt .skip) = .ok (some (integer 21)) := rfl

example :
    let body : ScalarBody [.optional .int, .optional .int] (.optional .int) :=
      .addOptional (.arg .first) (.arg (.next .first))
    let shifted := ScopedCalculation.call body (.cons vectorValues (.cons vectorReduction .nil))
    shifted.reduceWhole (sumInt .skip) = .ok (some (integer 21)) := rfl

example :
    let body : ScalarBody [.required .int] (.required .int) := .literal (integer 0)
    let failing : ScopedCalculation vectorScope RInt := .broadcast (fun _ => .error .arithmeticOverflow)
    ValuesAt.toList <$> (ScopedCalculation.call body (.cons failing .nil)).run vectorScope.members =
      .error .arithmeticOverflow := rfl

example :
    let body : ScalarBody [.required .int] (.required .int) := .literal (integer 0)
    let arguments : OrdinaryArguments (.vector 0) [.required .int] :=
      .cons (fun _ => .error .arithmeticOverflow) .nil
    (body.callOrdinary arguments []) = .error .arithmeticOverflow := rfl

example :
    let left : ScopedCalculation vectorScope (Option Bool) :=
      .pointwise (fun _ => .ok (some false))
    let right : ScopedCalculation vectorScope (Option Bool) :=
      .broadcast (fun _ => .error .arithmeticOverflow)
    ValuesAt.toList <$> (ScopedCalculation.boolean false left right).run vectorScope.members =
      .ok [some false, some false] := rfl

private def amountStage (stamp : Nat) (cells : Vector RInt 2) : Stage where
  domain := ⟨stamp, 2⟩
  schema name := if name = "amount" then some (.required .int) else none
  read := fun name t found row => by
    change (if name = "amount" then some (.required .int) else none) = some t at found
    split at found
    · cases found
      exact cells.get row
    · cases found

private def addCapturedOffset : DeferredColumn (.required .int) where
  captured := [.required .int]
  lexical := fun {_} param => match param with
    | .first => integer 1
  parameters := [.required .int, .required .int]
  inputs := .cons (.column "amount") (.cons (.lexical .first) .nil)
  body := plus

example :
    (addCapturedOffset.lower (amountStage 2 #v[integer 10, integer 20])).map
      (fun expression => expression.eval (0 : Fin 2)) = .ok (.ok (integer 11)) := rfl

example :
    (addCapturedOffset.lower (amountStage 3 #v[integer 30, integer 40])).map
      (fun expression => expression.eval (0 : Fin 2)) = .ok (.ok (integer 31)) := rfl

example :
    let source := amountStage 2 #v[integer 10, integer 20]
    let inclusion : RowInclusion source.domain ⟨4, 1⟩ :=
      ⟨fun _ => (1 : Fin 2), by
        intro a b order
        have oneRow := b.isLt
        change b.val < 1 at oneRow
        omega, by decide⟩
    (addCapturedOffset.lower (source.restrict inclusion)).map
      (fun expression => expression.eval (0 : Fin 1)) = .ok (.ok (integer 21)) := rfl

private def optionalInt := ContextType.calculation (Ty.optional .int)
private def sumNeeds := [Requirement.collection .group, .missing .unresolved]

private def forwardedSum : ContextTerm Ty.denote [optionalInt] optionalInt sumNeeds :=
  .apply (.apply (.forward sumNeeds optionalInt optionalInt) (.reducer .group .unresolved sumInt))
    (.variable .first)

private def sumEnvironment (context : ConsumptionContext) :
    ContextEnvironment Ty.denote context [optionalInt]
  | _, .first => .broadcast (fun _ => .ok (some (integer 2)))

private def acceptedContext : ConsumptionContext :=
  { scope := groupScope, policy := { verb := some .skip } }

example :
    (forwardedSum.run acceptedContext (sumEnvironment acceptedContext)).map
      (fun plan => ValuesAt.toList <$> plan.run groupScope.members) =
      .ok (.ok [some (integer 4), some (integer 4)]) := rfl

example :
    let context : ConsumptionContext := { scope := groupScope }
    forwardedSum.run context (sumEnvironment context) = .error (.missing .unresolved) := rfl

example :
    let context : ConsumptionContext := { scope := vectorScope, policy := { verb := some .skip } }
    forwardedSum.run context (sumEnvironment context) = .error (.collection .group) := rfl

def main : IO Unit := do
  for (id, result) in [("C52", c52), ("C56", c56), ("C119", c119), ("C123", c123),
      ("C124", c124)] do
    match result with
    | .ok value => IO.println (Lean.Json.arr #[.str id, .str value]).compress
    | .error failure => throw (IO.userError s!"{id}: {repr failure}")

run_cmd do
  let env ← Lean.getEnv
  let allowed := #[`propext, `Quot.sound, `Classical.choice]
  for (name, _) in env.constants do
    let check := match env.getModuleIdxFor? name with
      | none => true
      | some moduleId => (`Rupam : Lean.Name).isPrefixOf env.header.moduleNames[moduleId]!
    if check then
      for axiomName in ← Lean.collectAxioms name do
        unless allowed.contains axiomName do
          throwError "{name} depends on unapproved axiom {axiomName}"
