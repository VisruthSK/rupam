import Rupam.Deferred

namespace Rupam

-- A named selector retains source order and proves that each field exists.
inductive ResolvedNames (stage : Stage) : List String → Type where
  | nil : ResolvedNames stage []
  | cons (type : Ty) (present : stage.schema name = some type) :
      ResolvedNames stage rest → ResolvedNames stage (name :: rest)

def ResolvedNames.resolve (stage : Stage) :
    (names : List String) → Except ColumnError (ResolvedNames stage names)
  | [] => .ok .nil
  | name :: rest =>
    match found : stage.schema name with
    | none => .error (.absent name)
    | some type => (.cons type found ·) <$> resolve stage rest

def Stage.resolveSelector (stage : Stage) (names : List String) :
    Except ColumnError (ResolvedNames stage names.eraseDups) :=
  ResolvedNames.resolve stage names.eraseDups

theorem ResolvedNames.selected_exists (selected : ResolvedNames stage names) (seen : name ∈ names) :
    ∃ t, stage.schema name = some t := by
  induction selected with
  | nil => exact False.elim (List.not_mem_nil seen)
  | cons type present rest tail =>
    cases List.mem_cons.mp seen with
    | inl same =>
      subst name
      exact ⟨type, present⟩
    | inr member => exact tail member

def ResolvedNames.project {stage : Stage} {names : List String} (_ : ResolvedNames stage names) : Stage where
  domain := stage.domain
  schema name := if name ∈ names then stage.schema name else none
  read := fun name expected present row => by
    change (if name ∈ names then stage.schema name else none) = some expected at present
    by_cases included : name ∈ names
    · rw [ite_eq_left included] at present
      exact stage.read name expected present row
    · rw [ite_eq_right included] at present
      cases present

theorem ResolvedNames.project_schema (selected : ResolvedNames stage names)
    (name : String) (included : name ∈ names) :
    selected.project.schema name = stage.schema name := by
  simp [project, included]

theorem ResolvedNames.project_reads_selected (selected : ResolvedNames stage names)
    (name : String) (included : name ∈ names) (type : Ty)
    (original : stage.schema name = some type) (projected : selected.project.schema name = some type)
    (row : Row stage.domain) :
    selected.project.read name type projected row = stage.read name type original row := by
  simp [project, included]

end Rupam
