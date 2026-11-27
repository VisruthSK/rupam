import Init.Data.List.Nat.Pairwise
import Rupam.Lifting

namespace Rupam

inductive EvaluationScope where
  | vector (size : Nat)
  | group (group : Group domain)

def EvaluationScope.shape : EvaluationScope → Shape
  | .vector size => .vector size
  | @EvaluationScope.group domain _ => .column domain

abbrev EvaluationScope.Index (scope : EvaluationScope) := Position scope.shape

instance (scope : EvaluationScope) : DecidableEq scope.Index := by
  cases scope <;> exact inferInstanceAs (DecidableEq (Fin _))

def EvaluationScope.positions : (scope : EvaluationScope) → List scope.Index
  | .vector size => List.finRange size
  | .group witness => witness.rows

theorem EvaluationScope.positions_nodup (scope : EvaluationScope) :
    scope.positions.Nodup := by
  cases scope with
  | vector size => exact List.nodup_finRange size
  | group witness => exact witness.unique

abbrev EvaluationScope.Member (scope : EvaluationScope) :=
  { position : scope.Index // position ∈ scope.positions }

def EvaluationScope.members (scope : EvaluationScope) : List scope.Member :=
  scope.positions.attach

variable {scope : EvaluationScope}

-- A successful computation supplies a value at every requested row.
abbrev ValuesAt (rows : List scope.Member) (α : Type) :=
  (row : scope.Member) → row ∈ rows → α

variable {rows : List scope.Member}

namespace ValuesAt

def empty : ValuesAt ([] : List scope.Member) α :=
  fun _ member => False.elim (List.not_mem_nil member)

def collect : (rows : List scope.Member) →
    ((row : scope.Member) → row ∈ rows → Outcome α) → Outcome (ValuesAt rows α)
  | [], _ => .ok empty
  | first :: rest, read => do
    let firstValue ← read first List.mem_cons_self
    let restValues ← collect rest fun row member => read row (List.mem_cons_of_mem _ member)
    pure fun row member =>
      if isFirst : row = first then firstValue
      else restValues row ((List.mem_cons.mp member).resolve_left isFirst)

def toList (values : ValuesAt rows α) : List α :=
  rows.attach.map fun row => values row.val row.property

def select (values : ValuesAt rows α) (test : α → Bool) : List scope.Member :=
  rows.filter fun row =>
    if member : row ∈ rows then test (values row member) else false

theorem mem_select_iff (values : ValuesAt rows α) (test : α → Bool)
    (row : scope.Member) :
    row ∈ values.select test ↔ ∃ member : row ∈ rows, test (values row member) = true := by
  constructor
  · intro selected
    obtain ⟨member, passes⟩ := List.mem_filter.mp selected
    exact ⟨member, by simpa only [dite_eq_left member] using passes⟩
  · intro ⟨member, passes⟩
    exact List.mem_filter.mpr ⟨member, by simpa only [dite_eq_left member] using passes⟩

theorem mem_select (values : ValuesAt rows α) (test : α → Bool)
    (member : row ∈ rows) (selected : test (values row member) = true) :
    row ∈ values.select test :=
  (values.mem_select_iff test row).mpr ⟨member, selected⟩

def restrict (values : ValuesAt rows α) (selected : List scope.Member)
    (available : selected ⊆ rows) : ValuesAt selected α :=
  fun position member => values position (available member)

-- A guard's refinement can be read only at positions that passed its test.
def refine (values : ValuesAt rows α) (test : α → Bool) :
    ValuesAt (values.select test) {value : α // test value = true} :=
  fun position selected =>
    have passed := List.mem_filter.mp selected
    ⟨values position passed.1, by simpa only [dite_eq_left passed.1] using passed.2⟩

theorem guard_cannot_cover_whole (values : ValuesAt rows α) (test : α → Bool)
    (position : scope.Member) (inScope : position ∈ scope.members)
    (notSelected : position ∉ values.select test) :
    ¬ scope.members ⊆ values.select test := by
  intro coversWhole
  exact notSelected (coversWhole inScope)

theorem guard_covers_whole_iff (values : ValuesAt scope.members α) (test : α → Bool) :
    scope.members ⊆ values.select test ↔
      ∀ position member, test (values position member) = true := by
  constructor
  · intro covers position member
    obtain ⟨inScope, passed⟩ := (values.mem_select_iff test position).mp (covers member)
    exact passed
  · intro allPass position member
    exact values.mem_select test member (allPass position member)

theorem nested_selection (values : ValuesAt rows α) (outer inner : α → Bool)
    (position : scope.Member) :
    let outerRows := values.select outer
    let selected := values.restrict outerRows
      (fun {_} member => (List.mem_filter.mp member).1)
    position ∈ selected.select inner ↔
      ∃ member : position ∈ rows,
        outer (values position member) = true ∧ inner (values position member) = true := by
  dsimp
  rw [mem_select_iff]
  constructor
  · intro ⟨outerMember, passedInner⟩
    obtain ⟨member, passedOuter⟩ := (values.mem_select_iff outer position).mp outerMember
    exact ⟨member, passedOuter, passedInner⟩
  · intro ⟨member, passedOuter, passedInner⟩
    exact ⟨values.mem_select outer member passedOuter, passedInner⟩

theorem select_empty_iff (values : ValuesAt rows α) (test : α → Bool) :
    values.select test = [] ↔ ∀ row member, test (values row member) = false := by
  rw [select, List.filter_eq_nil_iff]
  constructor
  · intro noneSelected row member
    have notSelected := noneSelected row member
    simp only [dite_eq_left member] at notSelected
    exact Bool.eq_false_iff.mpr notSelected
  · intro noneSelected row member
    simp only [dite_eq_left member, noneSelected row member, Bool.false_eq_true, not_false_eq_true]

theorem collect_at {rows : List scope.Member}
    {read : (row : scope.Member) → row ∈ rows → Outcome α} {values : ValuesAt rows α}
    (computed : collect rows read = .ok values) (row : scope.Member) (member : row ∈ rows) :
    read row member = .ok (values row member) := by
  induction rows with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first rest tailCorrect =>
    rw [collect] at computed
    cases headRead : read first List.mem_cons_self with
    | error failure =>
      rw [headRead] at computed
      cases computed
    | ok firstValue =>
      rw [headRead] at computed
      cases tailRead : collect rest (fun r h => read r (List.mem_cons_of_mem _ h)) with
      | error failure =>
        rw [tailRead] at computed
        cases computed
      | ok tailValues =>
        rw [tailRead] at computed
        cases computed
        by_cases here : row = first
        · subst row
          simpa using headRead
        · have inTail := (List.mem_cons.mp member).resolve_left here
          simpa only [dite_eq_right here] using tailCorrect tailRead inTail

theorem collect_succeeds (rows : List scope.Member)
    (read : (position : scope.Member) → position ∈ rows → Outcome α)
    (succeeds : ∀ position member, ∃ value, read position member = .ok value) :
    ∃ values, collect rows read = .ok values := by
  induction rows with
  | nil => exact ⟨empty, rfl⟩
  | cons first rest tailSucceeds =>
    obtain ⟨firstValue, readFirst⟩ := succeeds first List.mem_cons_self
    let readRest := fun position member => read position (List.mem_cons_of_mem first member)
    have restSucceeds : ∀ position member, ∃ value, readRest position member = .ok value :=
      fun position member => succeeds position (List.mem_cons_of_mem first member)
    obtain ⟨restValues, readTail⟩ := tailSucceeds readRest restSucceeds
    rw [collect, readFirst]
    change (∃ values, (do
      let restValues ← collect rest readRest
      pure (fun position member =>
        if isFirst : position = first then firstValue
        else restValues position ((List.mem_cons.mp member).resolve_left isFirst))) = .ok values)
    rw [readTail]
    exact ⟨_, rfl⟩

end ValuesAt

-- The collection scope is fixed through composition. A branch changes only the active positions.
structure ScopedCalculation (scope : EvaluationScope) (α : Type) where
  runNonempty : (first : scope.Member) → (rest : List scope.Member) →
    Outcome (ValuesAt (first :: rest) α)

namespace ScopedCalculation

def run (computation : ScopedCalculation scope α) :
    (rows : List scope.Member) → Outcome (ValuesAt rows α)
  | [] => .ok ValuesAt.empty
  | first :: rest => computation.runNonempty first rest

theorem inactive (computation : ScopedCalculation scope α) :
    computation.run [] = .ok ValuesAt.empty := rfl

theorem succeeds_when_empty (computation : ScopedCalculation scope α) (empty : rows = []) :
    ∃ output, computation.run rows = .ok output := by
  subst rows
  exact ⟨ValuesAt.empty, computation.inactive⟩

theorem branch_inactive (branch : ScopedCalculation scope β) (values : ValuesAt rows α)
    (test : α → Bool) (noneSelected : ∀ row member, test (values row member) = false) :
    ∃ output, branch.run (values.select test) = .ok output := by
  exact branch.succeeds_when_empty ((values.select_empty_iff test).mpr noneSelected)

def pointwise (read : scope.Index → Outcome α) : ScopedCalculation scope α :=
  ⟨fun first rest => ValuesAt.collect (first :: rest) fun row _ => read row.val⟩

theorem pointwise_at (read : scope.Index → Outcome α) (rows : List scope.Member)
    {output : ValuesAt rows α}
    (evaluated : (pointwise read).run rows = .ok output) (row : scope.Member)
    (member : row ∈ rows) : read row.val = .ok (output row member) := by
  cases rows with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first rest => exact ValuesAt.collect_at evaluated row member

theorem pointwise_succeeds_iff (read : scope.Index → Outcome α) (rows : List scope.Member) :
    (∃ output, (pointwise read).run rows = .ok output) ↔
      ∀ row ∈ rows, ∃ value, read row.val = .ok value := by
  constructor
  · intro ⟨output, evaluated⟩ row member
    exact ⟨output row member, pointwise_at read rows evaluated row member⟩
  · intro each
    cases rows with
    | nil => exact ⟨ValuesAt.empty, rfl⟩
    | cons first rest => exact ValuesAt.collect_succeeds _ (fun row _ => read row.val) each

theorem pointwise_active_failure (read : scope.Index → Outcome α) (rows : List scope.Member)
    (row : scope.Member) (member : row ∈ rows) {failure : Failure}
    (failed : read row.val = .error failure) :
    ∃ failure, (pointwise read).run rows = .error failure := by
  cases outcome : (pointwise read).run rows with
  | error error => exact ⟨error, rfl⟩
  | ok values =>
    have readSucceeded := pointwise_at read rows outcome row member
    rw [failed] at readSucceeded
    cases readSucceeded

def broadcast (value : Unit → Outcome α) : ScopedCalculation scope α :=
  ⟨fun _ _ => do
    let scalar ← value ()
    pure fun _ _ => scalar⟩

def ofCore (expression : Expr scope.shape t) : ScopedCalculation scope t.denote :=
  pointwise expression.eval

def zipWith (left : ScopedCalculation scope α) (right : ScopedCalculation scope β)
    (combine : α → β → Outcome γ) : ScopedCalculation scope γ :=
  ⟨fun first rest => do
    let rows := first :: rest
    let leftValues ← left.run rows
    let rightValues ← right.run rows
    ValuesAt.collect rows fun row member => combine (leftValues row member) (rightValues row member)⟩

-- A transform reads the whole collection and returns a result indexed by its positions.
def transform (input : ScopedCalculation scope α)
    (operation : ValuesAt scope.members α → Outcome (scope.Member → β)) :
    ScopedCalculation scope β :=
  ⟨fun _ _ => do
    let values ← input.run scope.members
    let atRow ← operation values
    pure fun row _ => atRow row⟩

theorem transform_at (input : ScopedCalculation scope α)
    (operation : ValuesAt scope.members α → Outcome (scope.Member → β))
    (first : scope.Member) (rest : List scope.Member)
    {values : ValuesAt scope.members α} {atRow : scope.Member → β}
    (readGroup : input.run scope.members = .ok values) (transformed : operation values = .ok atRow) :
    (input.transform operation).run (first :: rest) = .ok (fun row _ => atRow row) := by
  change (do
    let values ← input.run scope.members
    let atRow ← operation values
    pure fun row _ => atRow row) = _
  rw [readGroup]
  change (do let result ← operation values; pure fun row _ => result row) = _
  rw [transformed]
  rfl

-- A demanded reduction has one result. Its input is evaluated over the whole
-- collection, including under a guard and when its input contains reductions.
def reduceWhole (input : ScopedCalculation scope α) (operation : List α → Outcome β) :
    Outcome β := do
  let values ← input.run scope.members
  operation values.toList

def reduce (input : ScopedCalculation scope α) (operation : List α → Outcome β) :
    ScopedCalculation scope β :=
  broadcast fun _ => input.reduceWhole operation

theorem reduce_demanded (input : ScopedCalculation scope α) (operation : List α → Outcome β)
    (first : scope.Member) (rest : List scope.Member) :
    (input.reduce operation).run (first :: rest) = (do
      let values ← input.run scope.members
      let scalar ← operation values.toList
      pure fun _ _ => scalar) := by
  simp only [reduce, reduceWhole, broadcast, run, bind_assoc]

theorem reduce_broadcast (input : ScopedCalculation scope α) (operation : List α → Outcome β)
    (first : scope.Member) (rest : List scope.Member)
    {values : ValuesAt scope.members α}
    (readGroup : input.run scope.members = .ok values)
    (reduced : operation values.toList = .ok value) :
    (input.reduce operation).run (first :: rest) = .ok (fun _ _ => value) := by
  rw [reduce_demanded, readGroup]
  change (do
    let scalar ← operation values.toList
    pure fun _ _ => scalar) = .ok (fun _ _ => value)
  rw [reduced]
  rfl

def ifThenElse (condition : ScopedCalculation scope Bool) (yes no : ScopedCalculation scope α) :
    ScopedCalculation scope α :=
  ⟨fun first rest => do
    let rows := first :: rest
    let flags ← condition.run rows
    let whenTrue ← yes.run (flags.select id)
    let whenFalse ← no.run (flags.select (! ·))
    pure fun row member =>
      if selected : flags row member = true then
        whenTrue row (flags.mem_select id member selected)
      else
        whenFalse row (flags.mem_select (! ·) member (by
          cases flag : flags row member with
          | false => rfl
          | true => exact False.elim (selected flag)))⟩

-- Use `id` for a required fallback and `some` for an optional fallback.
def coalesce (left : ScopedCalculation scope (Option α)) (right : ScopedCalculation scope β)
    (present : α → β) : ScopedCalculation scope β :=
  ⟨fun first rest => do
    let values ← left.run (first :: rest)
    let fallback ← right.run (values.select Option.isNone)
    pure fun row member =>
      match cell : values row member with
      | some value => present value
      | none =>
        fallback row (values.mem_select Option.isNone member (by rw [cell]; rfl))⟩

def boolean (decisive : Bool)
    (left right : ScopedCalculation scope (Option Bool)) : ScopedCalculation scope (Option Bool) :=
  ⟨fun first rest => do
    let values ← left.run (first :: rest)
    let needed := fun value : Option Bool => value != some decisive
    let fallback ← right.run (values.select needed)
    pure fun row member =>
      let value := values row member
      if demandsRight : needed value = true then
        combineTruth decisive value (fallback row (values.mem_select needed member demandsRight))
      else
        some decisive⟩

end ScopedCalculation

theorem ScalarBody.scope_lifting (body : ScalarBody parameters t)
    (arguments : ∀ {a}, Parameter parameters a → LiftArgument a scope.shape)
    (rows : List scope.Member) {output : ValuesAt rows t.denote}
    (evaluated : (ScopedCalculation.ofCore (body.lower (fun param => (arguments param).toCore))).run
      rows = .ok output) (row : scope.Member) (member : row ∈ rows) :
    body.eval (fun param => .ok ((arguments param).valueAt row.val)) =
      .ok (output row member) := by
  rw [← body.lifting arguments row.val]
  exact ScopedCalculation.pointwise_at _ rows evaluated row member

theorem ScalarBody.scope_lifting_succeeds_iff (body : ScalarBody parameters t)
    (arguments : ∀ {a}, Parameter parameters a → LiftArgument a scope.shape)
    (rows : List scope.Member) :
    (∃ output, (ScopedCalculation.ofCore (body.lower (fun param => (arguments param).toCore))).run
      rows = .ok output) ↔ ∀ row ∈ rows, ∃ value,
      body.eval (fun param => .ok ((arguments param).valueAt row.val)) = .ok value := by
  simp only [ScopedCalculation.ofCore, ScopedCalculation.pointwise_succeeds_iff, body.lifting]

end Rupam
