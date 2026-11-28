import Rupam.Scope

namespace Rupam

variable {scope : EvaluationScope} {rows : List scope.Member}

def ValuesAt.presentValues (values : ValuesAt rows (Option α)) :
    ValuesAt (values.select Option.isSome) α :=
  fun row selected =>
    let refined := values.refine Option.isSome row selected
    refined.val.get refined.property

theorem ValuesAt.presentValues_sound (values : ValuesAt rows (Option α))
    (row : scope.Member) (member : row ∈ rows) (selected : row ∈ values.select Option.isSome) :
    some (values.presentValues row selected) = values row member :=
  Option.some_get _

def matchOptionalScalar (subject : Outcome (Option α)) (whenPresent : α → Outcome β)
    (whenAbsent : Unit → Outcome β) : Outcome β := do
  match ← subject with
  | some value => whenPresent value
  | none => whenAbsent ()

private def optionalArms (values : ValuesAt rows (Option α))
    (present : ValuesAt (values.select Option.isSome) β)
    (absent : ValuesAt (values.select Option.isNone) β) : ValuesAt rows β :=
  fun row member =>
    match cell : values row member with
    | some _ => present row (values.mem_select Option.isSome member (by rw [cell]; rfl))
    | none => absent row (values.mem_select Option.isNone member (by rw [cell]; rfl))

def ScopedCalculation.matchOptional (subject : ScopedCalculation scope (Option α))
    (whenPresent : α → Outcome β) (whenAbsent : ScopedCalculation scope β) :
    ScopedCalculation scope β :=
  ⟨fun first rest => do
    let values ← subject.run (first :: rest)
    let present ← ValuesAt.collect (values.select Option.isSome) fun row member =>
      whenPresent (values.presentValues row member)
    let absent ← whenAbsent.run (values.select Option.isNone)
    pure (optionalArms values present absent)⟩

theorem ScopedCalculation.matchOptional_demand (subject : ScopedCalculation scope (Option α))
    (whenPresent : α → Outcome β) (whenAbsent : ScopedCalculation scope β)
    (first : scope.Member) (rest : List scope.Member)
    {values : ValuesAt (first :: rest) (Option α)}
    (checkedSubject : subject.run (first :: rest) = .ok values) :
    (subject.matchOptional whenPresent whenAbsent).run (first :: rest) = (do
      let present ← ValuesAt.collect (values.select Option.isSome) fun row member =>
        whenPresent (values.presentValues row member)
      let absent ← whenAbsent.run (values.select Option.isNone)
      pure (optionalArms values present absent)) := by
  change (do
    let checked ← subject.run (first :: rest)
    let present ← ValuesAt.collect (checked.select Option.isSome) fun row member =>
      whenPresent (checked.presentValues row member)
    let absent ← whenAbsent.run (checked.select Option.isNone)
    pure (optionalArms checked present absent)) = _
  rw [checkedSubject]
  rfl

def ValuesAt.withAbsent (flags : ValuesAt rows Bool)
    (selected : ValuesAt (flags.select id) α) (visible : α → Option β) : ValuesAt rows (Option β) :=
  fun row member =>
    if passes : flags row member = true then
      visible (selected row (flags.mem_select id member passes))
    else none

def ScopedCalculation.guard (selection : ScopedCalculation scope Bool)
    (body : ScopedCalculation scope α) (visible : α → Option β) : ScopedCalculation scope (Option β) :=
  ⟨fun first rest => do
    let flags ← selection.run (first :: rest)
    let selected ← body.run (flags.select id)
    pure (flags.withAbsent selected visible)⟩

theorem ScopedCalculation.guard_demand (selection : ScopedCalculation scope Bool)
    (body : ScopedCalculation scope α) (visible : α → Option β)
    (first : scope.Member) (rest : List scope.Member)
    {flags : ValuesAt (first :: rest) Bool}
    (readMask : selection.run (first :: rest) = .ok flags) :
    (selection.guard body visible).run (first :: rest) =
      (do
        let selected ← body.run (flags.select id)
        pure (flags.withAbsent selected visible)) := by
  change (do
    let available ← selection.run (first :: rest)
    let selected ← body.run (available.select id)
    pure (available.withAbsent selected visible)) = _
  rw [readMask]
  rfl

theorem ScopedCalculation.guard_skips_unselected (selection : ScopedCalculation scope Bool)
    (body : ScopedCalculation scope α) (visible : α → Option β)
    (first : scope.Member) (rest : List scope.Member)
    {flags : ValuesAt (first :: rest) Bool}
    (readMask : selection.run (first :: rest) = .ok flags)
    (noneSelected : ∀ row member, flags row member = false) :
    (selection.guard body visible).run (first :: rest) = .ok (fun _ _ => none) := by
  obtain ⟨unused, skipped⟩ := body.branch_inactive flags id noneSelected
  rw [selection.guard_demand body visible first rest readMask, skipped]
  change Except.ok (flags.withAbsent unused visible) = Except.ok (fun _ _ => none)
  congr 1
  funext row member
  simp [ValuesAt.withAbsent, noneSelected row member]

private theorem reduce_nonempty (input : ScopedCalculation scope α) (operation : List α → Outcome β)
    (selected : List scope.Member) (nonempty : selected ≠ []) :
    (input.reduce operation).run selected = (do
      let value ← input.reduceWhole operation
      pure (fun _ _ => value)) := by
  cases selected with
  | nil => exact False.elim (nonempty rfl)
  | cons _ _ => rfl

theorem ScopedCalculation.guard_reduces_full_scope (selection : ScopedCalculation scope Bool)
    (input : ScopedCalculation scope α) (operation : List α → Outcome β) (visible : β → Option γ)
    (first : scope.Member) (rest : List scope.Member) {flags : ValuesAt (first :: rest) Bool}
    (readMask : selection.run (first :: rest) = .ok flags)
    (activeHead : scope.Member) (activeTail : List scope.Member)
    (demanded : flags.select id = activeHead :: activeTail) :
    (selection.guard (input.reduce operation) visible).run (first :: rest) =
      (do
        let value ← input.reduceWhole operation
        pure (flags.withAbsent (fun _ _ => value) visible)) := by
  rw [selection.guard_demand (input.reduce operation) visible first rest readMask]
  have nonempty : flags.select id ≠ [] := by
    rw [demanded]
    exact List.cons_ne_nil _ _
  rw [reduce_nonempty input operation (flags.select id) nonempty]
  simp only [bind_assoc, pure_bind]

end Rupam
