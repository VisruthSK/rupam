import Rupam.HigherOrder
import Rupam.Elaboration

namespace Rupam

-- Data keep shape and cell type separate. Closure types record which call
-- rule applies and, for a contextual closure, what the caller must supply.
inductive QualifiedType where
  | data (shape : Shape) (cell : Ty)
  | calculation (result : Ty)
  | pointwise (input output : Ty)
  | contextual (latent : List Requirement) (input output : QualifiedType)
  deriving DecidableEq

def QualifiedType.denote (context : ConsumptionContext) : QualifiedType → Type
  | .data shape cell => (shape.valueType cell).denote
  | .calculation result => ScopedCalculation context.scope result.denote
  | .pointwise input output => input.denote → Outcome output.denote
  | .contextual latent input output =>
    Supplies context latent → input.denote context → output.denote context

def QualifiedType.Demand (context : ConsumptionContext) : QualifiedType → Type
  | .data shape _ => List (Position shape)
  | .calculation _ => List context.scope.Member
  | .pointwise .. | .contextual .. => Unit

def QualifiedType.Observation (context : ConsumptionContext) : QualifiedType → Type
  | .data _ cell | .calculation cell => List cell.denote
  | type@(.pointwise ..) => type.denote context
  | type@(.contextual ..) => type.denote context

private def positionedValue (shape : Shape) (cell : Ty) :
    (shape.valueType cell).denote → Position shape → cell.denote :=
  match shape with
  | .scalar => fun value _ => value
  | .vector _ => Vector.get
  | .column _ => fun read => read

def QualifiedType.observe (context : ConsumptionContext) :
    (type : QualifiedType) → type.denote context → type.Demand context →
      Outcome (type.Observation context)
  | .data shape cell, value, positions => .ok (positions.map (positionedValue shape cell value))
  | .calculation _, compute, positions => ValuesAt.toList <$> compute.run positions
  | .pointwise .., closure, _ | .contextual .., closure, _ => .ok closure

def ContextType.qualified : ContextType Ty → QualifiedType
  | .calculation result => .calculation result
  | .vectorValue size element => .data (.vector size) element
  | .function latent input output => .contextual latent input.qualified output.qualified

theorem ContextType.qualified_denote (type : ContextType Ty) (context : ConsumptionContext) :
    type.qualified.denote context = type.denote Ty.denote context := by
  induction type with
  | calculation => rfl
  | vectorValue => rfl
  | function latent input output inputSame outputSame =>
    simp only [qualified, QualifiedType.denote, denote, inputSame, outputSame]

inductive QualifiedApplies :
    QualifiedType → QualifiedType → QualifiedType → List Requirement → Bool → Type where
  | pointwise : CheckedCall [shape.valueType cell] [input] result →
      QualifiedApplies (.pointwise input output) (.data shape cell) (.data result output) [] true
  | calculation :
      QualifiedApplies (.pointwise input output) (.calculation input) (.calculation output) [] true
  | contextual :
      QualifiedApplies (.contextual latent input output) input output latent false

structure QualifiedApplication (function argument : QualifiedType) where
  output : QualifiedType
  latent : List Requirement
  pointwise : Bool
  proof : QualifiedApplies function argument output latent pointwise

def QualifiedApplies.infer (function argument : QualifiedType) :
    Option (QualifiedApplication function argument) :=
  match function, argument with
  | .pointwise input output, .data shape cell => do
    let ⟨result, call⟩ ← CheckedCall.infer [shape.valueType cell] [input]
    pure ⟨.data result output, [], true, .pointwise call⟩
  | .pointwise input output, .calculation actual =>
    if exact : actual = input then
      some ⟨.calculation output, [], true, exact.symm ▸ QualifiedApplies.calculation⟩
    else none
  | .contextual latent input output, argument =>
    if exact : argument = input then
      some ⟨output, latent, false, exact.symm ▸ QualifiedApplies.contextual⟩
    else none
  | _, _ => none

theorem QualifiedApplies.infer_complete (call : QualifiedApplies function argument output latent scalar) :
    infer function argument = some ⟨output, latent, scalar, call⟩ := by
  cases call with
  | pointwise checked => simp [infer, checked.infer_complete, bind, Option.bind, pure]
  | calculation => simp [infer]
  | contextual => simp [infer]

theorem QualifiedApplies.coherent
    (first : QualifiedApplies function argument output latent scalar)
    (second : QualifiedApplies function argument other otherLatent otherPointwise) :
    (⟨output, latent, scalar, first⟩ : QualifiedApplication function argument) =
      ⟨other, otherLatent, otherPointwise, second⟩ :=
  Option.some.inj (first.infer_complete.symm.trans second.infer_complete)

theorem QualifiedApplies.infer_rejects_iff (function argument : QualifiedType) :
    infer function argument = none ↔ ¬ Nonempty (QualifiedApplication function argument) := by
  constructor
  · intro rejected ⟨⟨_, _, _, call⟩⟩
    have accepted := call.infer_complete
    rw [rejected] at accepted
    cases accepted
  · intro impossible
    cases infer function argument with
    | none => rfl
    | some call => exact False.elim (impossible ⟨call⟩)

private def pointwiseBody {input output : Ty} (function : input.denote → Outcome output.denote) :
    ScalarBody [input] output :=
  .apply (.literal (t := .function input output) function) (.arg .first)

def QualifiedApplies.run (context : ConsumptionContext) :
    QualifiedApplies function argument result latent scalar →
      function.denote context → argument.denote context → Supplies context latent →
      result.Demand context → Outcome (result.Observation context)
  | .pointwise call, fn, value, _, positions =>
    call.runValues (pointwiseBody fn) (ArgumentValues.cons value ArgumentValues.nil) positions
  | .calculation (output := output), fn, input, _, positions =>
    (QualifiedType.calculation output).observe context (input.map fn) positions
  | .contextual (output := output), fn, value, supplies, positions =>
    output.observe context (fn supplies value) positions

theorem QualifiedApplies.run_pointwise {shape : Shape} {cell output : Ty}
    (call : CheckedCall [shape.valueType cell] [input] result) (context : ConsumptionContext)
    (function : input.denote → Outcome output.denote) (argument : (shape.valueType cell).denote)
    (positions : List (Position result)) :
    (QualifiedApplies.pointwise call).run context function argument .nil positions =
      positions.mapM (fun position =>
        function (call.passing.valuesAt call.alignment
          (ArgumentValues.cons argument ArgumentValues.nil) position .first)) := by
  simp only [run, CheckedCall.runValues_eq]
  rfl

theorem QualifiedApplies.run_calculation {input output : Ty} (context : ConsumptionContext)
    (function : input.denote → Outcome output.denote)
    (argument : ScopedCalculation context.scope input.denote)
    (demand : List context.scope.Member) :
    QualifiedApplies.calculation.run context function argument .nil demand =
      (ValuesAt.toList <$> (argument.map function).run demand) := rfl

theorem QualifiedApplies.run_contextual (context : ConsumptionContext)
    (function : (QualifiedType.contextual latent input output).denote context)
    (argument : input.denote context) (supplies : Supplies context latent)
    (positions : output.Demand context) :
    QualifiedApplies.contextual.run context function argument supplies positions =
      output.observe context (function supplies argument) positions := rfl

def QualifiedApplication.run (call : QualifiedApplication function argument)
    (context : ConsumptionContext) (fn : function.denote context) (value : argument.denote context)
    (demand : call.output.Demand context) :
    Except Requirement (Outcome (call.output.Observation context)) := do
  let supplied ← Supplies.discharge context call.latent
  pure (call.proof.run context fn value supplied demand)

theorem QualifiedApplication.run_requires_exactly (call : QualifiedApplication function argument)
    (context : ConsumptionContext) (fn : function.denote context) (value : argument.denote context)
    (demand : call.output.Demand context) :
    (∃ observation, call.run context fn value demand = .ok observation) ↔
      Nonempty (Supplies context call.latent) := by
  constructor
  · intro ⟨observation, ran⟩
    unfold run at ran
    cases checked : Supplies.discharge context call.latent with
    | error requirement => rw [checked] at ran; cases ran
    | ok supplied => exact ⟨supplied⟩
  · intro ⟨supplied⟩
    obtain ⟨checked, accepted⟩ := supplied.discharge_complete
    exact ⟨call.proof.run context fn value checked demand, by rw [run, accepted]; rfl⟩

def ContextTerm.runQualifiedApplication
    (function : ContextTerm Ty.denote locals (.function latent input output) functionNeeds)
    (argument : ContextTerm Ty.denote locals input argumentNeeds)
    (context : ConsumptionContext) (environment : ContextEnvironment Ty.denote context locals)
    (supplies : Supplies context (functionNeeds ++ (argumentNeeds ++ latent)))
    (positions : output.qualified.Demand context) :
    Outcome (output.qualified.Observation context) :=
  let (forFunction, rest) := supplies.split functionNeeds
  let (forArgument, forCall) := rest.split argumentNeeds
  let invoke : (QualifiedType.contextual latent input.qualified output.qualified).denote context :=
    fun caller value => cast (output.qualified_denote context).symm
      (function.eval context environment forFunction caller (cast (input.qualified_denote context) value))
  QualifiedApplies.contextual.run context invoke
    (cast (input.qualified_denote context).symm (argument.eval context environment forArgument))
    forCall positions

theorem ContextTerm.runQualifiedApplication_eq
    (function : ContextTerm Ty.denote locals (.function latent input output) functionNeeds)
    (argument : ContextTerm Ty.denote locals input argumentNeeds)
    (context : ConsumptionContext) (environment : ContextEnvironment Ty.denote context locals)
    (supplies : Supplies context (functionNeeds ++ (argumentNeeds ++ latent)))
    (positions : output.qualified.Demand context) :
    function.runQualifiedApplication argument context environment supplies positions =
      output.qualified.observe context
        (cast (output.qualified_denote context).symm
          ((ContextTerm.apply function argument).eval context environment supplies)) positions := by
  simp [runQualifiedApplication, QualifiedApplies.run, ContextTerm.eval]

structure QualifiedJudgment where
  type : QualifiedType
  needs : List Requirement
  scalarOnly : Bool
  deriving DecidableEq

def QualifiedJudgment.value (type : QualifiedType) : QualifiedJudgment := ⟨type, [], true⟩

def QualifiedJudgment.abstract (input : QualifiedType) (body : QualifiedJudgment) :
    QualifiedJudgment :=
  .value (if body.scalarOnly then
    match input, body.type, body.needs with
    | .data .scalar a, .data .scalar b, [] => .pointwise a b
    | _, _, _ => .contextual body.needs input body.type
  else .contextual body.needs input body.type)

def QualifiedApplication.judgment (function argument : QualifiedJudgment)
    (call : QualifiedApplication function.type argument.type) : QualifiedJudgment :=
  ⟨call.output, function.needs ++ argument.needs ++ call.latent,
    function.scalarOnly && argument.scalarOnly && call.pointwise⟩

inductive QualifiedNumbers : QualifiedType → QualifiedType → QualifiedType → Type where
  | int : IntOperands lt rt t →
      QualifiedNumbers (.data .scalar lt) (.data .scalar rt) (.data .scalar t)

def QualifiedNumbers.infer (left right : QualifiedType) :
    Option (Σ output, QualifiedNumbers left right output) :=
  match left, right with
  | .data .scalar lt, .data .scalar rt => do
    let ⟨type, numeric⟩ ← IntOperands.infer lt rt
    pure ⟨.data .scalar type, .int numeric⟩
  | _, _ => none

theorem QualifiedNumbers.infer_complete (numeric : QualifiedNumbers left right output) :
    infer left right = some ⟨output, numeric⟩ := by
  cases numeric with
  | int operands => simp [infer, operands.infer_complete, bind, Option.bind, pure]

def QualifiedJudgment.arithmetic (left right : QualifiedJudgment) (type : QualifiedType) :
    QualifiedJudgment :=
  ⟨type, left.needs ++ right.needs, left.scalarOnly && right.scalarOnly⟩

-- Names are resolved before this judgment. A signature may be supplied by a
-- primitive, an annotated declaration, or an already inferred definition.
inductive QualifiedSource where
  | reference (slot : Nat)
  | literal (type : Ty) (value : type.denote)
  | vector (element : Ty) (size : Nat) (values : Vector element.denote size)
  | contextualPrimitive (type : ContextType Ty) (value : ContextTerm Ty.denote [] type [])
  | lambda (body : QualifiedSource)
  | apply (function argument : QualifiedSource)
  | multiply (left right : QualifiedSource)
  | letIn (value body : QualifiedSource)

def QualifiedSource.generalizable : QualifiedSource → Bool
  | .reference _ | .literal .. | .vector .. | .contextualPrimitive .. | .lambda _ => true
  | .apply .. | .multiply .. | .letIn .. => false

-- These are the predicates of the scheme. Existentials bind the internal
-- interfaces of source subterms. The constraints contain no source syntax.
inductive Qualification where
  | truth
  | impossible
  | same (left right : QualifiedJudgment)
  | both (left right : Qualification)
  | existsType (under : QualifiedType → Qualification)
  | existsJudgment (under : QualifiedJudgment → Qualification)
  | application (function argument target : QualifiedJudgment)
  | arithmetic (left right target : QualifiedJudgment)

def Qualification.Holds : Qualification → Prop
  | .truth => True
  | .impossible => False
  | .same left right => left = right
  | .both left right => left.Holds ∧ right.Holds
  | .existsType under => ∃ type, (under type).Holds
  | .existsJudgment under => ∃ judgment, (under judgment).Holds
  | .application function argument target =>
    ∃ call : QualifiedApplication function.type argument.type, call.judgment function argument = target
  | .arithmetic left right target =>
    ∃ output, Nonempty (QualifiedNumbers left.type right.type output) ∧
      left.arithmetic right output = target

abbrev QualifiedBinding := QualifiedType → Qualification

def QualifiedBinding.mono (type : QualifiedType) : QualifiedBinding :=
  fun instanceType => .same (.value type) (.value instanceType)

-- The declarative rules use the call and primitive judgments directly.
inductive Qualifies : List QualifiedBinding → QualifiedSource → QualifiedJudgment → Prop where
  | reference (binding : QualifiedBinding) (found : lexical[slot]? = some binding)
      (instantiates : (binding type).Holds) :
      Qualifies lexical (.reference slot) (.value type)
  | literal (type : Ty) (value : type.denote) :
      Qualifies lexical (.literal type value) (.value (.data .scalar type))
  | vector (element : Ty) (size : Nat) (values : Vector element.denote size) :
      Qualifies lexical (.vector element size values) (.value (.data (.vector size) element))
  | contextualPrimitive (type : ContextType Ty) (value : ContextTerm Ty.denote [] type []) :
      Qualifies lexical (.contextualPrimitive type value) (.value type.qualified)
  | lambda :
      Qualifies (QualifiedBinding.mono input :: lexical) body result →
      Qualifies lexical (.lambda body) (result.abstract input)
  | apply (fn : Qualifies lexical function functionJudgment)
      (arg : Qualifies lexical argument argumentJudgment)
      (call : QualifiedApplication functionJudgment.type argumentJudgment.type) :
      Qualifies lexical (.apply function argument) (call.judgment functionJudgment argumentJudgment)
  | multiply (left : Qualifies lexical a leftJudgment)
      (right : Qualifies lexical b rightJudgment)
      (numeric : QualifiedNumbers leftJudgment.type rightJudgment.type output) :
      Qualifies lexical (.multiply a b) (leftJudgment.arithmetic rightJudgment output)
  | letIn (generalizable : value.generalizable = true) (binding : QualifiedBinding)
      (definition : Qualifies lexical value (.value witness))
      (instances : ∀ type, (binding type).Holds → Qualifies lexical value (.value type))
      (body : Qualifies (binding :: lexical) scope judgment) :
      Qualifies lexical (.letIn value scope) judgment

def QualifiedSource.generate (lexical : List QualifiedBinding) :
    QualifiedSource → QualifiedJudgment → Qualification
  | .reference slot, expected =>
    match lexical[slot]? with
    | none => .impossible
    | some binding => .existsType fun type => .both (binding type) (.same (.value type) expected)
  | .literal type _, expected => .same (.value (.data .scalar type)) expected
  | .vector element size _, expected => .same (.value (.data (.vector size) element)) expected
  | .contextualPrimitive type _, expected => .same (.value type.qualified) expected
  | .lambda body, expected => .existsType fun input => .existsJudgment fun result =>
    .both (body.generate (QualifiedBinding.mono input :: lexical) result)
      (.same (result.abstract input) expected)
  | .apply function argument, expected => .existsJudgment fun fn => .existsJudgment fun arg =>
    .both (function.generate lexical fn)
      (.both (argument.generate lexical arg) (.application fn arg expected))
  | .multiply left right, expected => .existsJudgment fun a => .existsJudgment fun b =>
    .both (left.generate lexical a)
      (.both (right.generate lexical b) (.arithmetic a b expected))
  | .letIn value body, expected =>
    if value.generalizable then
      let binding : QualifiedBinding := fun type => value.generate lexical (.value type)
      .existsType fun witness => .both (binding witness) (body.generate (binding :: lexical) expected)
    else .impossible

def QualifiedBinding.Subsumes (general specific : QualifiedBinding) : Prop :=
  ∀ type, (specific type).Holds → (general type).Holds

private def bindingsSubsume (general specific : List QualifiedBinding) : Prop :=
  ∀ (slot : Nat) binding, specific[slot]? = some binding →
    ∃ wider : QualifiedBinding, general[slot]? = some wider ∧ wider.Subsumes binding

private theorem bindingsSubsume.cons (rest : bindingsSubsume general specific)
    (head : wider.Subsumes narrow) : bindingsSubsume (wider :: general) (narrow :: specific) := by
  intro slot binding found
  cases slot with
  | zero =>
    cases Option.some.inj found
    exact ⟨wider, rfl, head⟩
  | succ slot => exact rest slot binding found

private theorem bindingsSubsume.refl (lexical : List QualifiedBinding) :
    bindingsSubsume lexical lexical :=
  fun _ binding found => ⟨binding, found, fun _ holds => holds⟩

private theorem QualifiedSource.generate_monotone (source : QualifiedSource)
    (wider : bindingsSubsume general specific)
    (satisfied : (source.generate specific judgment).Holds) :
    (source.generate general judgment).Holds := by
  induction source generalizing general specific judgment with
  | reference slot =>
    simp only [generate] at satisfied ⊢
    cases found : specific[slot]? with
    | none =>
      rw [found] at satisfied
      exact False.elim satisfied
    | some binding =>
      rw [found] at satisfied
      obtain ⟨widerBinding, widerFound, subsumes⟩ := wider _ _ found
      rw [widerFound]
      obtain ⟨type, constraints, same⟩ := satisfied
      exact ⟨type, subsumes type constraints, same⟩
  | literal | vector | contextualPrimitive => exact satisfied
  | lambda body monotone =>
    obtain ⟨input, result, constraints, same⟩ := satisfied
    exact ⟨input, result, monotone (wider.cons (fun _ holds => holds)) constraints, same⟩
  | apply function argument widerFunction widerArgument =>
    obtain ⟨fn, arg, functionConstraints, argumentConstraints, call⟩ := satisfied
    exact ⟨fn, arg, widerFunction wider functionConstraints, widerArgument wider argumentConstraints, call⟩
  | multiply left right widerLeft widerRight =>
    obtain ⟨a, b, leftConstraints, rightConstraints, numeric⟩ := satisfied
    exact ⟨a, b, widerLeft wider leftConstraints, widerRight wider rightConstraints, numeric⟩
  | letIn value body widerValue widerBody =>
    by_cases generalizable : value.generalizable = true
    · simp only [generate, generalizable, ite_true] at satisfied ⊢
      obtain ⟨type, definition, scope⟩ := satisfied
      exact ⟨type, widerValue wider definition, widerBody (wider.cons (fun _ => widerValue wider)) scope⟩
    · simp [generate, generalizable, Qualification.Holds] at satisfied

theorem Qualifies.satisfies (typed : Qualifies lexical source judgment) :
    (source.generate lexical judgment).Holds := by
  induction typed with
  | reference binding found instantiates =>
    rw [QualifiedSource.generate, found]
    exact ⟨_, instantiates, rfl⟩
  | literal | vector | contextualPrimitive => rfl
  | lambda _ body => exact ⟨_, _, body, rfl⟩
  | apply _ _ call function argument => exact ⟨_, _, function, argument, call, rfl⟩
  | multiply _ _ numeric left right => exact ⟨_, _, left, right, _, ⟨numeric⟩, rfl⟩
  | letIn generalizable _ _ _ _ definition instances body =>
    simp only [QualifiedSource.generate, generalizable, ite_true]
    exact ⟨_, definition, QualifiedSource.generate_monotone _
      (bindingsSubsume.cons (bindingsSubsume.refl _) instances) body⟩

theorem QualifiedSource.generation_sound (source : QualifiedSource)
    (lexical : List QualifiedBinding) (judgment : QualifiedJudgment)
    (satisfied : (source.generate lexical judgment).Holds) :
    Qualifies lexical source judgment := by
  induction source generalizing lexical judgment with
  | reference slot =>
    rw [generate] at satisfied
    cases found : lexical[slot]? with
    | none => rw [found] at satisfied; exact False.elim satisfied
    | some binding =>
      rw [found] at satisfied
      obtain ⟨type, instantiates, rfl⟩ := satisfied
      exact .reference binding found instantiates
  | literal type value =>
    change QualifiedJudgment.value (.data .scalar type) = judgment at satisfied
    exact satisfied ▸ Qualifies.literal type value
  | vector element size values =>
    change QualifiedJudgment.value (.data (.vector size) element) = judgment at satisfied
    exact satisfied ▸ Qualifies.vector element size values
  | contextualPrimitive type value =>
    change QualifiedJudgment.value type.qualified = judgment at satisfied
    exact satisfied ▸ Qualifies.contextualPrimitive type value
  | lambda body correct =>
    obtain ⟨input, result, typed, rfl⟩ := satisfied
    exact .lambda (correct _ _ typed)
  | apply function argument correctFunction correctArgument =>
    obtain ⟨fn, arg, functionTyped, argumentTyped, call, rfl⟩ := satisfied
    exact .apply (correctFunction _ _ functionTyped) (correctArgument _ _ argumentTyped) call
  | multiply left right correctLeft correctRight =>
    obtain ⟨a, b, leftTyped, rightTyped, _, ⟨numeric⟩, rfl⟩ := satisfied
    exact .multiply (correctLeft _ _ leftTyped) (correctRight _ _ rightTyped) numeric
  | letIn value body correctValue correctBody =>
    by_cases generalizable : value.generalizable = true
    · simp only [generate, generalizable, ite_true] at satisfied
      obtain ⟨type, definition, scope⟩ := satisfied
      exact .letIn generalizable _ (correctValue _ _ definition)
        (fun type constraints => correctValue _ (.value type) constraints) (correctBody _ _ scope)
    · simp [generate, generalizable, Qualification.Holds] at satisfied

-- A pure definition can be installed in the next lexical environment. Each
-- use independently supplies the witnesses for its own interface.
def QualifiedSource.binding (source : QualifiedSource) (lexical : List QualifiedBinding) :
    QualifiedBinding := fun type => source.generate lexical (.value type)

theorem QualifiedSource.binding_instances (source : QualifiedSource)
    (lexical : List QualifiedBinding) (type : QualifiedType) :
    (source.binding lexical type).Holds ↔ Qualifies lexical source (.value type) :=
  ⟨source.generation_sound lexical (.value type), Qualifies.satisfies⟩

theorem Qualifies.reference_mono
    (typed : Qualifies lexical (.reference slot) judgment)
    (found : lexical[slot]? = some (QualifiedBinding.mono type)) :
    judgment = .value type := by
  cases typed with
  | reference binding resolved instantiated =>
    cases Option.some.inj (resolved.symm.trans found)
    exact instantiated.symm

def QualifiedSource.square : QualifiedSource :=
  .lambda (.multiply (.reference 0) (.reference 0))

theorem QualifiedSource.square_scheme (judgment : QualifiedJudgment) :
    (square.generate [] judgment).Holds ↔
      ∃ input output, Nonempty (IntOperands input input output) ∧
        judgment = .value (.pointwise input output) := by
  constructor
  · intro holds
    have typed := square.generation_sound [] judgment holds
    cases typed with
    | lambda body =>
      cases body with
      | multiply left right numeric =>
        cases left.reference_mono (by rfl)
        cases right.reference_mono (by rfl)
        cases numeric with
        | int operands => exact ⟨_, _, ⟨operands⟩, rfl⟩
  · rintro ⟨input, output, ⟨operands⟩, rfl⟩
    have reference : Qualifies [.mono (.data .scalar input)] (.reference 0)
        (.value (.data .scalar input)) := .reference _ rfl rfl
    exact (Qualifies.lambda (.multiply reference reference (.int operands))).satisfies

def QualifiedSource.forwarder : QualifiedSource :=
  .lambda (.lambda (.apply (.reference 1) (.reference 0)))

theorem QualifiedSource.forwarder_scheme (judgment : QualifiedJudgment) :
    (forwarder.generate [] judgment).Holds ↔
      ∃ function argument, ∃ call : QualifiedApplication function argument,
        judgment = (((call.judgment (.value function) (.value argument)).abstract argument).abstract function) := by
  constructor
  · intro holds
    have typed := forwarder.generation_sound [] judgment holds
    cases typed with
    | lambda outer =>
      cases outer with
      | lambda inner =>
        cases inner with
        | apply fn arg call =>
          cases fn.reference_mono (by rfl)
          cases arg.reference_mono (by rfl)
          exact ⟨_, _, call, rfl⟩
  · rintro ⟨function, argument, call, rfl⟩
    have fn : Qualifies [.mono argument, .mono function] (.reference 1) (.value function) :=
      .reference _ rfl rfl
    have arg : Qualifies [.mono argument, .mono function] (.reference 0) (.value argument) :=
      .reference _ rfl rfl
    exact (Qualifies.lambda (.lambda (.apply fn arg call))).satisfies

theorem QualifiedSource.forwarder_retains_context (input output : QualifiedType)
    (latent : List Requirement) :
    let function := QualifiedType.contextual latent input output
    (forwarder.generate [] (.value (.contextual [] function function))).Holds := by
  apply (forwarder_scheme _).mpr
  exact ⟨_, input, ⟨output, latent, false, .contextual⟩, rfl⟩

theorem QualifiedSource.forwarder_retains_pointwise (input output : Ty) :
    let function := QualifiedType.pointwise input output
    (forwarder.generate [] (.value (.contextual [] function function))).Holds := by
  apply (forwarder_scheme _).mpr
  let call : CheckedCall [input] [input] .scalar :=
    ⟨[.scalar], .cons .whole .nil, .cons .scalar .nil⟩
  exact ⟨_, .data .scalar input, ⟨.data .scalar output, [], true, .pointwise call⟩, rfl⟩

-- Before the source rules fix a passing role, a forwarder can put the
-- lifting inside its returned closure or at the call to that closure.
theorem QualifiedSource.forwarder_lifting_placements (size : Nat) (input output : Ty) :
    let function := QualifiedType.pointwise input output
    let argument := QualifiedType.data (.vector size) input
    let result := QualifiedType.data (.vector size) output
    (forwarder.generate [] (.value (.contextual [] function function))).Holds ∧
    (forwarder.generate [] (.value (.contextual [] function (.contextual [] argument result)))).Holds := by
  constructor
  · exact forwarder_retains_pointwise input output
  · apply (forwarder_scheme _).mpr
    let call : CheckedCall [.vector size input] [input] (.vector size) :=
      ⟨[.vector size], .cons .vector .nil, .cons .vectorRight .nil⟩
    exact ⟨_, .data (.vector size) input,
      ⟨.data (.vector size) output, [], true, .pointwise call⟩, rfl⟩

-- The same source judgment specializes both definitions. The contextual
-- consumer retains its requirements until the lifted argument is supplied.
private theorem QualifiedSource.forwarder_consumes_lifted
    (numeric : IntOperands element element cell)
    (argument : ∀ lexical, Qualifies lexical operand (.value actual))
    (operation : ContextTerm Ty.denote [] (.function latent receiver output) [])
    (lifting : QualifiedApplies (.pointwise element cell) actual receiver.qualified [] true) :
    let reducer := QualifiedSource.contextualPrimitive _ operation
    let program := QualifiedSource.letIn square (.letIn forwarder
      (.apply (.apply (.reference 0) reducer) (.apply (.reference 1) operand)))
    (program.generate [] ⟨output.qualified, latent, false⟩).Holds := by
  let sq := QualifiedSource.square.binding []
  let fw := QualifiedSource.forwarder.binding [sq]
  let input := receiver.qualified
  let function := QualifiedType.contextual latent input output.qualified
  have squareWitness : (sq (.pointwise element cell)).Holds :=
    (square_scheme _).mpr ⟨_, _, ⟨numeric⟩, rfl⟩
  have forwardWitness : (fw (.contextual [] function function)).Holds :=
    QualifiedSource.generate_monotone _ (by intro _ _ found; cases found)
      (forwarder_retains_context input output.qualified latent)
  have squareRef : Qualifies [fw, sq] (.reference 1) (.value (.pointwise element cell)) :=
    .reference sq rfl squareWitness
  have forwardRef : Qualifies [fw, sq] (.reference 0) (.value (.contextual [] function function)) :=
    .reference fw rfl forwardWitness
  have lifted : Qualifies [fw, sq] (.apply (.reference 1) operand) (.value input) :=
    .apply squareRef (argument _) ⟨_, [], true, lifting⟩
  have forwarded : Qualifies [fw, sq] (.apply (.reference 0) (.contextualPrimitive _ operation))
      ⟨function, [], false⟩ :=
    .apply forwardRef (.contextualPrimitive _ operation) ⟨_, [], false, .contextual⟩
  have applied := Qualifies.apply forwarded lifted ⟨output.qualified, latent, false, .contextual⟩
  exact (Qualifies.letIn rfl sq (square.generation_sound _ _ squareWitness)
    (fun _ => square.generation_sound _ _) (.letIn rfl fw
      (forwarder.generation_sound _ _ forwardWitness)
      (fun _ => forwarder.generation_sound _ _) applied)).satisfies

theorem QualifiedSource.forwarder_consumes_lifted_vector
    (numeric : IntOperands element element cell) (values : Vector element.denote size)
    (operation : ContextTerm Ty.denote [] (.function latent (.vectorValue size cell) output) []) :
    let reducer := QualifiedSource.contextualPrimitive _ operation
    let vector := QualifiedSource.vector element size values
    let program := QualifiedSource.letIn square (.letIn forwarder
      (.apply (.apply (.reference 0) reducer) (.apply (.reference 1) vector)))
    (program.generate [] ⟨output.qualified, latent, false⟩).Holds :=
  forwarder_consumes_lifted numeric (fun _ => .vector element size values) operation
    (.pointwise ⟨[.vector size], .cons .vector .nil, .cons .vectorRight .nil⟩)

theorem QualifiedSource.forwarder_consumes_lifted_calculation
    (numeric : IntOperands element element cell)
    (scopedArgument : ContextTerm Ty.denote [] (.calculation element) [])
    (operation : ContextTerm Ty.denote [] (.function latent (.calculation cell) output) []) :
    let reducer := QualifiedSource.contextualPrimitive _ operation
    let argument := QualifiedSource.contextualPrimitive _ scopedArgument
    let program := QualifiedSource.letIn square (.letIn forwarder
      (.apply (.apply (.reference 0) reducer) (.apply (.reference 1) argument)))
    (program.generate [] ⟨output.qualified, latent, false⟩).Holds :=
  forwarder_consumes_lifted numeric (fun _ => .contextualPrimitive _ scopedArgument) operation .calculation

end Rupam
