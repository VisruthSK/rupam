import Rupam.Restriction
import Init.Data.List.Nat.Pairwise

namespace Rupam

def filterDecision (policy : MissingPolicy) : Option Bool → Outcome Bool
  | some answer => .ok answer
  | none =>
    match policy with
    | .skip => .ok false
    | .keep => .ok true
    | .error => .error .missingValue

structure RowSelection (predicate : α → Outcome Bool) (input : List α) where
  rows : List α
  subsequence : rows.Sublist input
  retained : ∀ row, row ∈ rows ↔ row ∈ input ∧ predicate row = .ok true

variable {α : Type} {predicate : α → Outcome Bool} {rest : List α}

private def RowSelection.keep (tail : RowSelection predicate rest) (head : α)
    (passed : predicate head = .ok true) : RowSelection predicate (head :: rest) where
  rows := head :: tail.rows
  subsequence := .cons_cons head tail.subsequence
  retained row := by
    rw [List.mem_cons, List.mem_cons, tail.retained row]
    constructor
    · intro retained
      cases retained with
      | inl same =>
        exact ⟨Or.inl same, same.symm ▸ passed⟩
      | inr checked =>
        exact ⟨Or.inr checked.1, checked.2⟩
    · intro ⟨input, passed⟩
      exact input.elim Or.inl (fun member => Or.inr ⟨member, passed⟩)

private def RowSelection.drop (tail : RowSelection predicate rest) (head : α)
    (failed : predicate head = .ok false) : RowSelection predicate (head :: rest) where
  rows := tail.rows
  subsequence := .cons head tail.subsequence
  retained row := by
    rw [tail.retained row, List.mem_cons]
    constructor
    · intro ⟨member, passed⟩
      exact ⟨Or.inr member, passed⟩
    · intro ⟨input, passed⟩
      cases input with
      | inl same =>
        subst row
        rw [failed] at passed
        cases passed
      | inr member =>
        exact ⟨member, passed⟩

def selectRows (predicate : α → Outcome Bool) :
    (input : List α) → Outcome (RowSelection predicate input)
  | [] => .ok ⟨[], .slnil, by simp⟩
  | head :: rest =>
    match decided : predicate head with
    | .error failure => .error failure
    | .ok true => (·.keep head decided) <$> selectRows predicate rest
    | .ok false => (·.drop head decided) <$> selectRows predicate rest

structure FilterResult (source : Stage) (stamp : Nat) (decideRow : Row source.domain → Outcome Bool) where
  selected : RowSelection decideRow (List.finRange source.domain.size)
  inclusion : RowInclusion source.domain ⟨stamp, selected.rows.length⟩
  enumeratesSelection : ∀ row, inclusion.sourceRow row = selected.rows[row.val]

def FilterResult.stage (result : FilterResult source stamp decideRow) : Stage :=
  source.restrict result.inclusion

def Stage.filter (source : Stage) (stamp : Nat) (fresh : stamp ≠ source.domain.identity)
    (predicate : Expr (.column source.domain) (.optional .bool)) (policy : MissingPolicy) :
    Outcome (FilterResult source stamp (fun row => do filterDecision policy (← predicate.eval row))) := do
  let selected ← selectRows _ (List.finRange source.domain.size)
  let ordered := List.Pairwise.sublist selected.subsequence (List.pairwise_lt_finRange source.domain.size)
  let inclusion : RowInclusion source.domain ⟨stamp, selected.rows.length⟩ :=
    { sourceRow := fun row => selected.rows[row.val]
      ordered := fun {a b} before => List.pairwise_iff_getElem.mp ordered
        a.val b.val a.isLt b.isLt before
      fresh }
  pure ⟨selected, inclusion, fun _ => rfl⟩

theorem FilterResult.retained_exactly (result : FilterResult source stamp decideRow)
    (row : Row source.domain) :
    row ∈ result.selected.rows ↔ decideRow row = .ok true := by
  rw [result.selected.retained row]
  exact ⟨And.right, fun passed => ⟨List.mem_finRange row, passed⟩⟩

theorem FilterResult.skip_retained_iff {source : Stage}
    (predicate : Expr (.column source.domain) (.optional .bool))
    (result : FilterResult source stamp (fun row => do filterDecision .skip (← predicate.eval row)))
    (row : Row source.domain) :
    row ∈ result.selected.rows ↔ predicate.eval row = .ok (some true) := by
  rw [result.retained_exactly row]
  cases predicate.eval row with
  | error failure => simp [bind, Except.bind]
  | ok answer =>
    cases answer <;> simp [bind, Except.bind, filterDecision]

theorem FilterResult.skip_refines_output {source : Stage}
    (predicate : Expr (.column source.domain) (.optional .bool))
    (result : FilterResult source stamp (fun row => do filterDecision .skip (← predicate.eval row)))
    (property : Row source.domain → Prop)
    (fromPredicate : ∀ row, predicate.eval row = .ok (some true) → property row)
    (row : Row result.stage.domain) : property (result.inclusion.sourceRow row) := by
  apply fromPredicate
  apply (result.skip_retained_iff predicate _).mp
  rw [result.enumeratesSelection row]
  exact List.getElem_mem _

end Rupam
