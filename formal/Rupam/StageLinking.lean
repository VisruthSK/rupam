import Rupam.Deferred
import Rupam.SourceSemantics

namespace Rupam

variable {context : ConsumptionContext}

def DeferredReference.readAt (reference : DeferredReference captured t)
    (lexical : ArgumentValues captured) (stage : Stage) :
    reference.Available stage → Row stage.domain → t.denote :=
  match reference with
  | .lexical identity => fun _ _ => lexical identity
  | .column name => fun present row => stage.read name t present row

theorem DeferredReference.linked_reads (reference : DeferredReference captured t)
    (lexical : ArgumentValues captured) (stage : Stage)
    (available : reference.Available stage) {argument : LiftArgument t (.column stage.domain)}
    (linked : reference.bind lexical stage = .ok argument) (row : Row stage.domain) :
    argument.valueAt row = reference.readAt lexical stage available row := by
  cases reference with
  | lexical identity =>
    have value := Except.ok.inj (linked.symm.trans (lexical_stable identity lexical stage))
    rw [value]
    rfl
  | column name =>
    have value := Except.ok.inj (linked.symm.trans (column_at name lexical stage available))
    rw [value]
    rfl

def DeferredColumn.Ready (closure : DeferredColumn t) (stage : Stage) : Prop :=
  ∀ {a} (parameter : Parameter closure.parameters a), (closure.inputs.reference parameter).Available stage

-- This semantics performs a stage-relative read at an occurrence. The source
-- check supplies availability, including for references in inactive arms.
def DeferredColumn.directAt (closure : DeferredColumn t) (stage : Stage)
    (ready : closure.Ready stage) (row : Row stage.domain) : Outcome t.denote :=
  closure.body.eval fun param =>
    .ok ((closure.inputs.reference param).readAt closure.lexical stage (ready param) row)

theorem DeferredColumn.linked_reads (closure : DeferredColumn t) (stage : Stage)
    (ready : closure.Ready stage) {core : Expr (.column stage.domain) t}
    (linked : closure.lower stage = .ok core) (row : Row stage.domain) :
    core.eval row = closure.directAt stage ready row := by
  unfold DeferredColumn.lower at linked
  cases found : closure.inputs.bind stage closure.lexical with
  | error problem =>
    rw [found] at linked
    cases linked
  | ok arguments =>
    rw [found] at linked
    have sameCore :
        closure.body.lower (fun param => (arguments.argument param).toCore) = core :=
      Except.ok.inj linked
    rw [← sameCore, closure.body.lifting arguments.argument row]
    unfold directAt
    congr 1
    funext _ param
    exact congrArg Except.ok ((closure.inputs.reference param).linked_reads closure.lexical
      stage (ready param) (closure.inputs.bind_at stage closure.lexical found param) row)

theorem DeferredColumn.link_succeeds (closure : DeferredColumn t) (stage : Stage)
    (ready : closure.Ready stage) : ∃ core, closure.lower stage = .ok core := by
  obtain ⟨arguments, linked⟩ := (closure.inputs.bind_succeeds_iff stage closure.lexical).mpr ready
  refine ⟨closure.body.lower (fun param => (arguments.argument param).toCore), ?_⟩
  unfold lower
  rw [linked]
  rfl

theorem DeferredColumn.link_succeeds_iff (closure : DeferredColumn t) (stage : Stage) :
    (∃ core, closure.lower stage = .ok core) ↔ closure.Ready stage := by
  constructor
  · intro ⟨core, lowered⟩
    unfold lower at lowered
    cases bound : closure.inputs.bind stage closure.lexical with
    | error failure =>
      rw [bound] at lowered
      cases lowered
    | ok inputs =>
      intro _ parameter
      exact ((closure.inputs.bind_succeeds_iff stage closure.lexical).mp ⟨inputs, bound⟩) parameter
  · exact closure.link_succeeds stage

theorem Expr.eval_at_equal_shape (term : Expr actual t) (same : expected = actual)
    (position : Position expected) :
    (Eq.mp (congrArg (fun shape => Expr shape t) same.symm) term).eval position =
      term.eval (Eq.mp (congrArg Position same) position) := by
  cases same
  rfl

private def ofAlignedColumn (scope : EvaluationScope) (same : scope.shape = .column domain)
    (term : Expr (.column domain) t) : ScopedCalculation scope t.denote :=
  ScopedCalculation.ofCore (t := t) (Eq.mp (congrArg (fun shape => Expr shape t) same.symm) term)

-- Ordinary values cross the call boundary as values. A deferred argument
-- has a distinct type and is linked in the stage that consumes it.
inductive StageInputs (context : ConsumptionContext) : List (ContextType Ty) → Type where
  | nil : StageInputs context []
  | ordinary : t.denote Ty.denote context → StageInputs context rest → StageInputs context (t :: rest)
  | deferred : DeferredColumn t → StageInputs context rest → StageInputs context (.calculation t :: rest)

structure StageEnvironment (context : ConsumptionContext) (locals : List (ContextType Ty)) where
  lookup : ContextEnvironment Ty.denote context locals

private def StageEnvironment.empty : StageEnvironment context [] :=
  ⟨fun {_} binding => nomatch binding⟩

private def StageEnvironment.extend (environment : StageEnvironment context locals)
    (value : t.denote Ty.denote context) : StageEnvironment context (t :: locals) :=
  ⟨ContextEnvironment.push (payload := Ty.denote) (context := context) (input := t)
    value environment.lookup⟩

def StageInputs.Ready (stage : Stage) : StageInputs context locals → Prop
  | .nil => True
  | .ordinary _ rest => rest.Ready stage
  | .deferred closure rest => closure.Ready stage ∧ rest.Ready stage

def StageInputs.direct (stage : Stage) (aligned : context.scope.shape = .column stage.domain) :
    (arguments : StageInputs context locals) → arguments.Ready stage →
      StageEnvironment context locals
  | .nil, _ => .empty
  | .ordinary value rest, ready => (rest.direct stage aligned ready).extend value
  | .deferred closure rest, ready =>
    (rest.direct stage aligned ready.2).extend (t := .calculation _)
      (ScopedCalculation.pointwise (fun position =>
        closure.directAt stage ready.1 (Eq.mp (congrArg Position aligned) position)))

def StageInputs.link (stage : Stage) (aligned : context.scope.shape = .column stage.domain) :
    StageInputs context locals → Except ColumnError (StageEnvironment context locals)
  | .nil => .ok .empty
  | .ordinary value rest => do
    let tail ← rest.link stage aligned
    pure (tail.extend value)
  | .deferred closure rest => do
    let core ← closure.lower stage
    let tail ← rest.link stage aligned
    pure (tail.extend (t := .calculation _) (ofAlignedColumn context.scope aligned core))

theorem StageInputs.link_agrees (arguments : StageInputs context locals) (stage : Stage)
    (aligned : context.scope.shape = .column stage.domain) (ready : arguments.Ready stage) :
    arguments.link stage aligned = .ok (arguments.direct stage aligned ready) := by
  induction arguments with
  | nil => rfl
  | ordinary value rest linkedRest =>
    rw [link, linkedRest ready]
    rfl
  | deferred closure rest linkedRest =>
    obtain ⟨core, linked⟩ := closure.link_succeeds stage ready.1
    have present :
        ofAlignedColumn context.scope aligned core =
          ScopedCalculation.pointwise (fun position =>
            closure.directAt stage ready.1 (Eq.mp (congrArg Position aligned) position)) := by
      unfold ofAlignedColumn ScopedCalculation.ofCore
      congr 1
      funext position
      rw [Expr.eval_at_equal_shape core aligned position]
      exact closure.linked_reads stage ready.1 linked (Eq.mp (congrArg Position aligned) position)
    rw [link, linked, linkedRest ready.2]
    change Except.ok ((rest.direct stage aligned ready.2).extend (t := .calculation _)
      (ofAlignedColumn context.scope aligned core)) = Except.ok _
    rw [present] <;> rfl

theorem StageInputs.link_succeeds_iff (arguments : StageInputs context locals) (stage : Stage)
    (aligned : context.scope.shape = .column stage.domain) :
    (∃ environment, arguments.link stage aligned = .ok environment) ↔ arguments.Ready stage := by
  constructor
  · intro ⟨environment, linked⟩
    induction arguments with
    | nil => trivial
    | ordinary _ rest readyRest =>
      unfold link at linked
      cases found : rest.link stage aligned with
      | error failure =>
        rw [found] at linked
        cases linked
      | ok tail =>
        rw [found] at linked
        exact readyRest tail found
    | deferred closure rest readyRest =>
      unfold link at linked
      cases found : closure.lower stage with
      | error failure =>
        rw [found] at linked
        cases linked
      | ok core =>
        rw [found] at linked
        simp only [bind, Except.bind] at linked
        refine ⟨(closure.link_succeeds_iff stage).mp ⟨core, found⟩, ?_⟩
        cases restFound : rest.link stage aligned with
        | error failure =>
          rw [restFound] at linked
          cases linked
        | ok tail =>
          rw [restFound] at linked
          exact readyRest tail restFound
  · intro ready
    exact ⟨arguments.direct stage aligned ready, arguments.link_agrees stage aligned ready⟩

def ContextTerm.linkedRun (term : ContextTerm Ty.denote locals result needs)
    (arguments : StageInputs context locals) (stage : Stage)
    (aligned : context.scope.shape = .column stage.domain) :
    Except ColumnError (Except Requirement (result.denote Ty.denote context)) := do
  let environment ← arguments.link stage aligned
  pure (term.run context environment.lookup)

-- Equality is at the type of the helper result. For an analytical
-- calculation it includes execution on every active mask, not one sample.
theorem ContextTerm.eliminate_deferred_lookup (term : ContextTerm Ty.denote locals result needs)
    (arguments : StageInputs context locals) (stage : Stage)
    (aligned : context.scope.shape = .column stage.domain) (ready : arguments.Ready stage) :
    term.linkedRun arguments stage aligned =
      .ok (term.run context (arguments.direct stage aligned ready).lookup) := by
  unfold linkedRun
  rw [arguments.link_agrees stage aligned ready]
  rfl

theorem ContextElaborates.link_preserves_source
    (derivation : ContextElaborates locals source result needs)
    (arguments : StageInputs context locals) (stage : Stage)
    (aligned : context.scope.shape = .column stage.domain) (ready : arguments.Ready stage) :
    derivation.core.linkedRun arguments stage aligned =
      .ok (derivation.runSource context (arguments.direct stage aligned ready).lookup) := by
  rw [derivation.core.eliminate_deferred_lookup arguments stage aligned ready,
    derivation.run_preserves_source]

end Rupam
