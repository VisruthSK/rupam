import Rupam.Core

namespace Rupam

def fillMissing (value fallback : Option α) : Option α :=
  value <|> fallback

-- `inputs` contains the evaluated left operands at the active rows.
def Group.completeCoalesce (group : Group d) (inputs : List (Row d × Option α))
    (fallback : List (Row d) → Outcome (Option α)) : Outcome (List (Row d × Option α)) :=
  if inputs.any (fun (_, value) => value.isNone) then do
    let replacement ← fallback group.rows
    pure (inputs.map fun (row, value) => (row, fillMissing value replacement))
  else
    .ok inputs

private def readCells (rows : List (Row d)) (left : Row d → Outcome (Option α)) :
    Outcome (List (Row d × Option α)) :=
  rows.mapM fun row => do
    let value ← left row
    pure (row, value)

private theorem readCells_cons (row : Row d) (tail : List (Row d))
    (left : Row d → Outcome (Option α)) :
    readCells (row :: tail) left = (do
      let value ← left row
      let rest ← readCells tail left
      pure ((row, value) :: rest)) := by
  simp only [readCells, List.mapM_cons, bind_assoc, pure_bind]

private theorem readCells_preserve_rows (rows : List (Row d))
    (left : Row d → Outcome (Option α)) (read : readCells rows left = .ok inputs) :
    inputs.map Prod.fst = rows := by
  induction rows generalizing inputs with
  | nil =>
    cases read
    rfl
  | cons row tail tailRows =>
    rw [readCells_cons] at read
    cases head : left row with
    | error failure =>
      rw [head] at read
      cases read
    | ok value =>
      rw [head] at read
      cases rest : readCells tail left with
      | error failure =>
        rw [rest] at read
        cases read
      | ok cells =>
        rw [rest] at read
        have cellsInOrder : (row, value) :: cells = inputs := Except.ok.inj read
        rw [← cellsInOrder, List.map_cons, tailRows rest]

def Group.coalesce (group : Group d) (active : Row d → Bool)
    (left : Row d → Outcome (Option α)) (fallback : List (Row d) → Outcome (Option α)) :
    Outcome (List (Row d × Option α)) := do
  let inputs ← readCells (group.rows.filter active) left
  group.completeCoalesce inputs fallback

theorem Group.completeCoalesce_when_present
    (group : Group d) (inputs : List (Row d × Option α))
    (fallback : List (Row d) → Outcome (Option α))
    (allPresent : inputs.any (fun (_, value) => value.isNone) = false) :
    group.completeCoalesce inputs fallback = .ok inputs := by
  -- With no missing consumer, the reduction is never needed.
  unfold Group.completeCoalesce
  rw [allPresent]
  rfl

theorem Group.completeCoalesce_when_missing
    (group : Group d) (inputs : List (Row d × Option α))
    (fallback : List (Row d) → Outcome (Option α))
    (hasMissing : inputs.any (fun (_, value) => value.isNone) = true) :
    group.completeCoalesce inputs fallback = (do
      let replacement ← fallback group.rows
      pure (inputs.map fun (row, value) => (row, fillMissing value replacement))) := by
  -- The existence of one missing cell demands the reduction on the whole group.
  unfold Group.completeCoalesce
  rw [hasMissing]
  rfl

theorem Group.coalesce_inactive (group : Group d) (active : Row d → Bool)
    (left : Row d → Outcome (Option α)) (fallback : List (Row d) → Outcome (Option α))
    (inactive : group.rows.filter active = []) :
    group.coalesce active left fallback = .ok [] := by
  unfold Group.coalesce
  rw [inactive]
  rfl

theorem Group.coalesce_left_failure (group : Group d) (active : Row d → Bool)
    (left : Row d → Outcome (Option α)) (fallback : List (Row d) → Outcome (Option α))
    (failure : Failure)
    (failed : (group.rows.filter active).mapM (fun row => do
      let value ← left row
      pure (row, value)) = .error failure) :
    group.coalesce active left fallback = .error failure := by
  unfold Group.coalesce readCells
  rw [failed]
  rfl

theorem fillMissing_preserves_present (value : α) (fallback : Option α) :
    fillMissing (some value) fallback = some value := rfl

theorem Group.coalesce_preserves_rows (group : Group d) (active : Row d → Bool)
    (left : Row d → Outcome (Option α)) (fallback : List (Row d) → Outcome (Option α))
    (evaluated : group.coalesce active left fallback = .ok output) :
    output.map Prod.fst = group.rows.filter active := by
  unfold Group.coalesce at evaluated
  cases read : readCells (group.rows.filter active) left with
  | error failure =>
    rw [read] at evaluated
    cases evaluated
  | ok inputs =>
    have inputRows := readCells_preserve_rows _ left read
    rw [read] at evaluated
    change group.completeCoalesce inputs fallback = .ok output at evaluated
    cases missing : inputs.any (fun (_, value) => value.isNone) with
    | false =>
      -- Every left value survives, in the order in which it was read.
      rw [group.completeCoalesce_when_present inputs fallback missing] at evaluated
      cases evaluated
      exact inputRows
    | true =>
      rw [group.completeCoalesce_when_missing inputs fallback missing] at evaluated
      cases reduced : fallback group.rows with
      | error failure =>
        rw [reduced] at evaluated
        cases evaluated
      | ok replacement =>
        rw [reduced] at evaluated
        cases evaluated
        -- Filling the cells changes the values but preserves their row IDs.
        simpa only [List.map_map, Function.comp_def] using inputRows

end Rupam
