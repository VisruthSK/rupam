import Rupam.Patterns
import Rupam.Guarded

namespace Rupam

variable {scope : EvaluationScope} {rows : List scope.Member}
variable {input output subject result : Type}

-- A pattern binding is available at the rows that matched it. A captured
-- calculation still has access to its own enclosing collection.
structure BoundCalculation (scope : EvaluationScope) (input output : Type) where
  runNonempty : (first : scope.Member) → (rest : List scope.Member) →
    ValuesAt (first :: rest) input → Outcome (ValuesAt (first :: rest) output)

def BoundCalculation.run (calculation : BoundCalculation scope input output) :
    (rows : List scope.Member) → ValuesAt rows input → Outcome (ValuesAt rows output)
  | [], _ => .ok ValuesAt.empty
  | first :: rest, bindings => calculation.runNonempty first rest bindings

def BoundCalculation.pointwise (f : input → Outcome output) : BoundCalculation scope input output :=
  ⟨fun first rest bindings => ValuesAt.collect (first :: rest) fun row member =>
    f (bindings row member)⟩

def BoundCalculation.closed (calculation : ScopedCalculation scope output) :
    BoundCalculation scope input output :=
  ⟨fun first rest _ => calculation.run (first :: rest)⟩

theorem BoundCalculation.pointwise_observes (f : input → Outcome output)
    (bindings : ValuesAt rows input) {values : ValuesAt rows output}
    (ran : (BoundCalculation.pointwise f).run rows bindings = .ok values)
    (row : scope.Member) (member : row ∈ rows) :
    f (bindings row member) = .ok (values row member) := by
  cases rows with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first rest => exact ValuesAt.collect_at ran row member

theorem BoundCalculation.inactive (calculation : BoundCalculation scope input output)
    (bindings : ValuesAt ([] : List scope.Member) input) :
    calculation.run [] bindings = .ok ValuesAt.empty := rfl

theorem BoundCalculation.closed_reduction_scope (input : ScopedCalculation scope α)
    (operation : List α → Outcome β) (first : scope.Member) (rest : List scope.Member)
    (bindings : ValuesAt (first :: rest) captures) :
    (BoundCalculation.closed (input.reduce operation)).run (first :: rest) bindings =
      (do
        let value ← input.reduceWhole operation
        pure fun _ _ => value) := rfl

structure ScopedPatternArm (scope : EvaluationScope) (subject result : Type) where
  bindings : Type
  pattern : Pattern subject bindings
  guard : BoundCalculation scope bindings Bool
  body : BoundCalculation scope bindings result

namespace ScopedPatternArm

def bindingCandidates (arm : ScopedPatternArm scope subject result) (subjects : ValuesAt rows subject) :
    ValuesAt rows (Option arm.bindings) :=
  fun row member => arm.pattern.matchValue (subjects row member)

def candidates (arm : ScopedPatternArm scope subject result) (subjects : ValuesAt rows subject) :
    List scope.Member :=
  (arm.bindingCandidates subjects).select Option.isSome

def candidateBindings (arm : ScopedPatternArm scope subject result) (subjects : ValuesAt rows subject) :
    ValuesAt (arm.candidates subjects) arm.bindings :=
  (arm.bindingCandidates subjects).presentValues

theorem candidate_bindings_match (arm : ScopedPatternArm scope subject result)
    (subjects : ValuesAt rows subject) (row : scope.Member) (member : row ∈ rows)
    (candidate : row ∈ arm.candidates subjects) :
    arm.pattern.Matches (subjects row member) (arm.candidateBindings subjects row candidate) :=
  arm.pattern.matchValue_sound _ _
    ((arm.bindingCandidates subjects).presentValues_sound row member candidate).symm

theorem candidates_iff (arm : ScopedPatternArm scope subject result)
    (subjects : ValuesAt rows subject) (row : scope.Member) :
    row ∈ arm.candidates subjects ↔
      ∃ member : row ∈ rows, ∃ bindings, arm.pattern.Matches (subjects row member) bindings := by
  constructor
  · intro candidate
    obtain ⟨member, _⟩ := ((arm.bindingCandidates subjects).mem_select_iff Option.isSome row).mp candidate
    exact ⟨member, arm.candidateBindings subjects row candidate,
      arm.candidate_bindings_match subjects row member candidate⟩
  · intro ⟨member, bindings, matched⟩
    apply (arm.bindingCandidates subjects).mem_select Option.isSome member
    change (arm.pattern.matchValue (subjects row member)).isSome = true
    rw [matched.evaluates]
    rfl

private def selectedBindings (arm : ScopedPatternArm scope subject result)
    (subjects : ValuesAt rows subject) (flags : ValuesAt (arm.candidates subjects) Bool) :
    ValuesAt (flags.select id) arm.bindings :=
  (arm.candidateBindings subjects).restrict _ (fun {_} member => (List.mem_filter.mp member).1)

structure Execution (arm : ScopedPatternArm scope subject result) (subjects : ValuesAt rows subject) where
  flags : ValuesAt (arm.candidates subjects) Bool
  values : ValuesAt (flags.select id) result

def run (arm : ScopedPatternArm scope subject result) (subjects : ValuesAt rows subject) :
    Outcome (Execution arm subjects) := do
  let flags ← arm.guard.run (arm.candidates subjects) (arm.candidateBindings subjects)
  let values ← arm.body.run (flags.select id) (arm.selectedBindings subjects flags)
  pure ⟨flags, values⟩

theorem run_success_iff (arm : ScopedPatternArm scope subject result) (subjects : ValuesAt rows subject)
    (execution : Execution arm subjects) :
    arm.run subjects = .ok execution ↔
      arm.guard.run (arm.candidates subjects) (arm.candidateBindings subjects) = .ok execution.flags ∧
      arm.body.run (execution.flags.select id) (arm.selectedBindings subjects execution.flags) =
        .ok execution.values := by
  constructor
  · intro accepted
    unfold run at accepted
    cases guardRead : arm.guard.run (arm.candidates subjects) (arm.candidateBindings subjects) with
    | error failure =>
      rw [guardRead] at accepted
      cases accepted
    | ok flags =>
      rw [guardRead] at accepted
      simp only [bind, Except.bind] at accepted
      cases bodyRead : arm.body.run (flags.select id) (arm.selectedBindings subjects flags) with
      | error failure =>
        rw [bodyRead] at accepted
        cases accepted
      | ok values =>
        rw [bodyRead] at accepted
        cases accepted
        exact ⟨rfl, bodyRead⟩
  · intro ⟨guardRead, bodyRead⟩
    unfold run
    rw [guardRead]
    simp only [bind, Except.bind]
    rw [bodyRead]
    cases execution
    rfl

theorem run_empty (arm : ScopedPatternArm scope subject result)
    (subjects : ValuesAt ([] : List scope.Member) subject) :
    arm.run subjects = .ok ⟨ValuesAt.empty, ValuesAt.empty⟩ := rfl

variable {arm : ScopedPatternArm scope subject result} {subjects : ValuesAt rows subject}

def Execution.remaining (execution : Execution arm subjects) : List scope.Member :=
  rows.filter (fun row => decide (row ∉ execution.flags.select id))

theorem Execution.remaining_iff (execution : Execution arm subjects) (row : scope.Member) :
    row ∈ execution.remaining ↔ row ∈ rows ∧ row ∉ execution.flags.select id := by
  simp [remaining]

private def Execution.remainingSubjects (execution : Execution arm subjects) :
    ValuesAt execution.remaining subject :=
  subjects.restrict _ (fun {row} member => ((execution.remaining_iff row).mp member).1)

private def Execution.combine (execution : Execution arm subjects)
    (later : ValuesAt execution.remaining (Option result)) : ValuesAt rows (Option result) :=
  fun row member =>
    if selected : row ∈ execution.flags.select id then some (execution.values row selected)
    else later row ((execution.remaining_iff row).mpr ⟨member, selected⟩)

-- The optional result records positions still unmatched after the last arm.
-- Exhaustiveness is a separate obligation of an ordinary source match.
def runOrdered : List (ScopedPatternArm scope subject result) →
    (rows : List scope.Member) → ValuesAt rows subject → Outcome (ValuesAt rows (Option result))
  | [], _, _ => .ok (fun _ _ => none)
  | arm :: later, _, subjects => do
    let selected ← arm.run subjects
    let unmatched ← runOrdered later selected.remaining selected.remainingSubjects
    pure (selected.combine unmatched)

theorem runOrdered_step (arm : ScopedPatternArm scope subject result)
    (later : List (ScopedPatternArm scope subject result)) (subjects : ValuesAt rows subject)
    (selected : Execution arm subjects) (firstRan : arm.run subjects = .ok selected) :
    runOrdered (arm :: later) rows subjects = (do
      let rest ← runOrdered later selected.remaining selected.remainingSubjects
      pure (selected.combine rest)) := by
  rw [runOrdered, firstRan]
  rfl

theorem runOrdered_empty (arms : List (ScopedPatternArm scope subject result))
    (subjects : ValuesAt ([] : List scope.Member) subject) :
    runOrdered arms [] subjects = .ok ValuesAt.empty := by
  induction arms generalizing subjects with
  | nil =>
    apply congrArg Except.ok
    funext row member
    exact False.elim (List.not_mem_nil member)
  | cons arm later inductionStep =>
    let execution : Execution arm subjects := ⟨ValuesAt.empty, ValuesAt.empty⟩
    have inactive := runOrdered_step arm later subjects execution (arm.run_empty subjects)
    have remainingEmpty :
        runOrdered later execution.remaining execution.remainingSubjects = .ok ValuesAt.empty :=
      inductionStep execution.remainingSubjects
    rw [inactive, remainingEmpty]
    apply congrArg Except.ok
    funext row member
    exact False.elim (List.not_mem_nil member)

theorem runOrdered_keeps_selected_result (arm : ScopedPatternArm scope subject result)
    (later : List (ScopedPatternArm scope subject result)) (subjects : ValuesAt rows subject)
    (selected : Execution arm subjects) (firstRan : arm.run subjects = .ok selected)
    (output : ValuesAt rows (Option result)) (ran : runOrdered (arm :: later) rows subjects = .ok output)
    (row : scope.Member) (member : row ∈ rows) (chosen : row ∈ selected.flags.select id) :
    output row member = some (selected.values row chosen) := by
  rw [runOrdered_step arm later subjects selected firstRan] at ran
  cases restRan : runOrdered later selected.remaining selected.remainingSubjects with
  | error failure =>
    rw [restRan] at ran
    cases ran
  | ok rest =>
    rw [restRan] at ran
    cases ran
    simp [Execution.combine, chosen]

-- A checked ordinary match cannot finish successfully with unmatched rows.
-- A failure in an evaluated guard or arm is still an observable outcome.
abbrev Covers (arms : List (ScopedPatternArm scope subject result)) : Prop :=
  ∀ (rows : List scope.Member) (subjects : ValuesAt rows subject)
    (output : ValuesAt rows (Option result)),
    runOrdered arms rows subjects = .ok output →
      ∀ row member, (output row member).isSome

theorem Covers.prepend {later : List (ScopedPatternArm scope subject result)}
    (covered : Covers later) (arm : ScopedPatternArm scope subject result) :
    Covers (arm :: later) := by
  intro rows subjects output ran row member
  cases firstRan : arm.run subjects with
  | error failure =>
    rw [runOrdered, firstRan] at ran
    cases ran
  | ok selected =>
    rw [runOrdered_step arm later subjects selected firstRan] at ran
    cases laterRan : runOrdered later selected.remaining selected.remainingSubjects with
    | error failure =>
      rw [laterRan] at ran
      cases ran
    | ok rest =>
      rw [laterRan] at ran
      cases ran
      by_cases chosen : row ∈ selected.flags.select id
      · simp [Execution.combine, chosen]
      · simp only [Execution.combine, dite_eq_right chosen]
        exact covered selected.remaining selected.remainingSubjects rest laterRan row
          ((selected.remaining_iff row).mpr ⟨member, chosen⟩)

def otherwise (body : ScopedCalculation scope result) : ScopedPatternArm scope subject result :=
  { bindings := Unit
    pattern := .wildcard
    guard := .pointwise (fun _ => .ok true)
    body := .closed body }

theorem otherwise_covers (body : ScopedCalculation scope result) :
    Covers ([otherwise body] : List (ScopedPatternArm scope subject result)) := by
  intro rows subjects output ran row member
  let arm : ScopedPatternArm scope subject result := otherwise body
  change runOrdered [arm] rows subjects = .ok output at ran
  cases firstRan : arm.run subjects with
  | error failure =>
    rw [runOrdered, firstRan] at ran
    cases ran
  | ok selected =>
    have candidate : row ∈ arm.candidates subjects :=
      (arm.bindingCandidates subjects).mem_select Option.isSome member rfl
    have guardRan := ((arm.run_success_iff subjects selected).mp firstRan).1
    have passed := BoundCalculation.pointwise_observes (fun _ : Unit => .ok true)
      (arm.candidateBindings subjects) guardRan row candidate
    have chosen : row ∈ selected.flags.select id :=
      selected.flags.mem_select id candidate (Except.ok.inj passed).symm
    rw [runOrdered_keeps_selected_result arm [] subjects selected firstRan output ran row member chosen]
    rfl

theorem covers_with_fallback (earlier : List (ScopedPatternArm scope subject result))
    (body : ScopedCalculation scope result) : Covers (earlier ++ [otherwise body]) := by
  induction earlier with
  | nil => exact otherwise_covers body
  | cons arm _ covered => exact covered.prepend arm

end ScopedPatternArm

def ScopedCalculation.scopedMatch (subject : ScopedCalculation scope input)
    (arms : List (ScopedPatternArm scope input result)) (covered : ScopedPatternArm.Covers arms) :
    ScopedCalculation scope result :=
  ⟨fun first rest => do
    let rows := first :: rest
    let subjects ← subject.run rows
    match ran : ScopedPatternArm.runOrdered arms rows subjects with
    | .error failure => .error failure
    | .ok output => .ok (fun row member => (output row member).get (covered rows subjects output ran row member))⟩

theorem ScopedCalculation.scopedMatch_preserves_result (subject : ScopedCalculation scope input)
    (arms : List (ScopedPatternArm scope input result)) (covered : ScopedPatternArm.Covers arms)
    (first : scope.Member) (rest : List scope.Member)
    {subjects : ValuesAt (first :: rest) input} {output : ValuesAt (first :: rest) (Option result)}
    (subjectRan : subject.run (first :: rest) = .ok subjects)
    (armsRan : ScopedPatternArm.runOrdered arms (first :: rest) subjects = .ok output)
    (row : scope.Member) (member : row ∈ first :: rest) :
    ∃ values, (subject.scopedMatch arms covered).run (first :: rest) = .ok values ∧
      some (values row member) = output row member := by
  refine ⟨(fun position belongs =>
    (output position belongs).get (covered (first :: rest) subjects output armsRan position belongs)), ?_, ?_⟩
  · unfold ScopedCalculation.run ScopedCalculation.scopedMatch
    dsimp only
    rw [subjectRan]
    simp only [bind, Except.bind]
    split
    · rename_i failure failed
      rw [armsRan] at failed
      cases failed
    · rename_i observed observation
      have same := Except.ok.inj (observation.symm.trans armsRan)
      subst observed
      rfl
  · exact Option.some_get _

end Rupam
