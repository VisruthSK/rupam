import Rupam.AnalyticalPipeline
import Rupam.ScopedMatch

namespace Rupam

-- A checked stage plan can be used at any mask selected by the enclosing match.
structure CheckedStagePlan (scope : EvaluationScope) (t : Ty) where
  call : StageCall t
  sameScope : call.context.scope = scope
  contextAvailable : call.directPlan.toOption.isSome = true

namespace CheckedStagePlan

def direct (plan : CheckedStagePlan scope t) : ScopedCalculation scope t.denote :=
  plan.sameScope ▸ plan.call.directPlan.toOption.get plan.contextAvailable

def linked (plan : CheckedStagePlan scope t) : ScopedCalculation scope t.denote :=
  plan.sameScope ▸ (plan.call.linkedPlan.toOption.bind Except.toOption).get (by
    rw [plan.call.linkedPlan_eq]
    exact plan.contextAvailable)

theorem elimination_preserves_plan (plan : CheckedStagePlan scope t) :
    plan.linked = plan.direct := by
  unfold linked direct
  simp only [plan.call.linkedPlan_eq, Except.toOption, Option.bind_some]

end CheckedStagePlan

-- Build plans from the actual bound values at the demanded rows. With an empty
-- mask, BoundCalculation.run does not request a plan.
abbrev StagePlanFor (scope : EvaluationScope) (input : Type) (t : Ty) :=
  (first : scope.Member) → (rest : List scope.Member) →
    ValuesAt (first :: rest) input → CheckedStagePlan scope t

def StagePlanFor.direct (plans : StagePlanFor scope input t) :
    BoundCalculation scope input t.denote :=
  ⟨fun first rest bindings => (plans first rest bindings).direct.run (first :: rest)⟩

def StagePlanFor.linked (plans : StagePlanFor scope input t) :
    BoundCalculation scope input t.denote :=
  ⟨fun first rest bindings => (plans first rest bindings).linked.run (first :: rest)⟩

theorem StagePlanFor.elimination_preserves_binding (plans : StagePlanFor scope input t) :
    plans.linked = plans.direct := by
  unfold linked direct
  congr 1
  funext first rest bindings
  rw [(plans first rest bindings).elimination_preserves_plan]

structure StagePatternArm (scope : EvaluationScope) (subject : Type) (t : Ty) where
  bindings : Type
  pattern : Pattern subject bindings
  guard : StagePlanFor scope bindings (.required .bool)
  body : StagePlanFor scope bindings t

def StagePatternArm.direct (arm : StagePatternArm scope subject t) :
    ScopedPatternArm scope subject t.denote :=
  ⟨arm.bindings, arm.pattern, arm.guard.direct, arm.body.direct⟩

def StagePatternArm.linked (arm : StagePatternArm scope subject t) :
    ScopedPatternArm scope subject t.denote :=
  ⟨arm.bindings, arm.pattern, arm.guard.linked, arm.body.linked⟩

theorem StagePatternArm.elimination_preserves_arm (arm : StagePatternArm scope subject t) :
    arm.linked = arm.direct := by
  simp only [linked, direct, StagePlanFor.elimination_preserves_binding]

theorem StagePatternArm.elimination_preserves_arms (arms : List (StagePatternArm scope subject t)) :
    arms.map linked = arms.map direct := by
  induction arms with
  | nil => rfl
  | cons arm rest preserved =>
    simp only [List.map_cons, arm.elimination_preserves_arm, preserved]

theorem StagePatternArm.elimination_preserves_match
    (arms : List (StagePatternArm scope subject t)) (subjectValue : ScopedCalculation scope subject)
    (directCovers : ScopedPatternArm.Covers (arms.map direct))
    (linkedCovers : ScopedPatternArm.Covers (arms.map linked)) :
    subjectValue.scopedMatch (arms.map linked) linkedCovers =
      subjectValue.scopedMatch (arms.map direct) directCovers := by
  have sameArms {left right : List (ScopedPatternArm scope subject t.denote)}
      (same : left = right) (leftCovers : ScopedPatternArm.Covers left)
      (rightCovers : ScopedPatternArm.Covers right) :
      subjectValue.scopedMatch left leftCovers = subjectValue.scopedMatch right rightCovers := by
    subst right
    rfl
  exact sameArms (elimination_preserves_arms arms) linkedCovers directCovers

end Rupam
