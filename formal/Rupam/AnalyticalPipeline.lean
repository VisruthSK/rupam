import Rupam.SelectiveLinking

namespace Rupam

structure StageCall (t : Ty) where
  source : Stage
  context : ConsumptionContext
  aligned : context.scope.shape = .column source.domain
  locals : List (ContextType Ty)
  needs : List Requirement
  arguments : StageInputs context locals
  sourceTerm : ContextualSource
  derivation : ContextElaborates locals sourceTerm (.calculation t) needs
  ready : arguments.ReadyFor source derivation.core.uses
  unused : StageEnvironment context locals
  active : List context.scope.Member

inductive StageFailure where
  | schema (error : ColumnError)
  | context (missing : Requirement)
  | calculation (error : Failure)

private def StageCall.execute (call : StageCall t)
    (plan : Except Requirement (ScopedCalculation call.context.scope t.denote)) :
    Except StageFailure (ValuesAt call.active t.denote) := do
  let calculation ← plan.mapError .context
  (calculation.run call.active).mapError .calculation

def StageCall.direct (call : StageCall t) : Except StageFailure (ValuesAt call.active t.denote) :=
  let used := call.derivation.core.uses
  let arguments := call.arguments.forUses used call.unused
  let ready := (call.arguments.forUses_ready_iff call.source used call.unused).mpr call.ready
  let sourceBindings := arguments.direct call.source call.aligned ready
  call.execute (call.derivation.runSource call.context sourceBindings.lookup)

def StageCall.linked (call : StageCall t) : Except StageFailure (ValuesAt call.active t.denote) := do
  let arguments := call.arguments.forUses call.derivation.core.uses call.unused
  let plan ← (call.derivation.core.linkedRun arguments call.source call.aligned).mapError .schema
  call.execute plan

theorem StageCall.elimination_preserves_execution (call : StageCall t) : call.linked = call.direct := by
  unfold linked direct
  dsimp only
  rw [call.derivation.link_uses_preserves_source call.arguments call.source call.aligned
    call.unused call.unused call.ready]
  rfl

theorem StageCall.direct_ignores_unused (call : StageCall t)
    (other : StageEnvironment call.context call.locals) :
    call.direct = ({ call with unused := other }).direct := by
  have first := call.derivation.link_uses_preserves_source call.arguments call.source call.aligned
    call.unused call.unused call.ready
  have second := call.derivation.link_uses_preserves_source call.arguments call.source call.aligned
    call.unused other call.ready
  exact congrArg call.execute (Except.ok.inj (first.symm.trans second))

-- Each continuation can establish a new consuming schema and row domain from
-- the previous result. Its arguments must be checked at that new stage.
inductive AnalyticalPipeline (result : Type) where
  | done (value : result)
  | call (operation : StageCall t)
      (next : ValuesAt operation.active t.denote → AnalyticalPipeline result)

def AnalyticalPipeline.direct : AnalyticalPipeline result → Except StageFailure result
  | .done value => .ok value
  | .call operation next => do
    let values ← operation.direct
    (next values).direct

def AnalyticalPipeline.linked : AnalyticalPipeline result → Except StageFailure result
  | .done value => .ok value
  | .call operation next => do
    let values ← operation.linked
    (next values).linked

theorem AnalyticalPipeline.elimination_preserves_execution (pipeline : AnalyticalPipeline result) :
    pipeline.linked = pipeline.direct := by
  induction pipeline with
  | done value => rfl
  | call operation next preserved =>
    simp only [linked, direct, operation.elimination_preserves_execution, preserved]

end Rupam
