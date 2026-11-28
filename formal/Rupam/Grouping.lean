import Rupam.Scope
import Init.Data.Vector.OfFn

namespace Rupam

-- Key equality and missingness are supplied by the checked key type. For a
-- composite key, missingness means that at least one component is absent.
structure Grouping (domain : Domain) (key : Type) where
  entryKey : Row domain → key
  accepts : Row domain → Bool

def Grouping.start (keys : Row domain → key) (missing : key → Bool) :
    MissingPolicy → Outcome (Grouping domain key)
  | .skip => .ok ⟨keys, fun row => !missing (keys row)⟩
  | .keep => .ok ⟨keys, fun _ => true⟩
  | .error =>
    if (List.finRange domain.size).any (fun row => missing (keys row)) then .error .missingValue
    else .ok ⟨keys, fun _ => true⟩

def Grouping.group (partition : Grouping domain key) [DecidableEq key] (identity : key) :
    Group domain :=
  ⟨(List.finRange domain.size).filter (fun row =>
      partition.accepts row && decide (partition.entryKey row = identity)),
    List.Sublist.nodup List.filter_sublist (List.nodup_finRange domain.size)⟩

theorem Grouping.member_iff (partition : Grouping domain key) [DecidableEq key]
    (identity : key) (row : Row domain) :
    row ∈ (partition.group identity).rows ↔
      partition.accepts row = true ∧ partition.entryKey row = identity := by
  simp [group, List.mem_finRange]

theorem Grouping.accepted_has_group (partition : Grouping domain key) [DecidableEq key]
    (row : Row domain) (accepted : partition.accepts row = true) :
    row ∈ (partition.group (partition.entryKey row)).rows :=
  (partition.member_iff _ row).mpr ⟨accepted, rfl⟩

theorem Grouping.unique_group (partition : Grouping domain key) [DecidableEq key]
    (row : Row domain) (first second : key)
    (inFirst : row ∈ (partition.group first).rows)
    (inSecond : row ∈ (partition.group second).rows) : first = second := by
  have firstKey := ((partition.member_iff first row).mp inFirst).2
  have secondKey := ((partition.member_iff second row).mp inSecond).2
  exact firstKey.symm.trans secondKey

def Grouping.keys (partition : Grouping domain key) [DecidableEq key] : List key :=
  ((List.finRange domain.size).filter partition.accepts |>.map partition.entryKey).eraseDups

theorem Grouping.observed_keys (partition : Grouping domain key) [DecidableEq key]
    (identity : key) : identity ∈ partition.keys ↔ ∃ row, row ∈ (partition.group identity).rows := by
  unfold keys
  rw [List.mem_eraseDups]
  constructor
  · intro observed
    obtain ⟨row, kept, keyAtRow⟩ := List.mem_map.mp observed
    exact ⟨row, (partition.member_iff identity row).mpr
      ⟨(List.mem_filter.mp kept).2, keyAtRow⟩⟩
  · intro ⟨row, member⟩
    obtain ⟨accepted, keyAtRow⟩ := (partition.member_iff identity row).mp member
    exact List.mem_map.mpr ⟨row, List.mem_filter.mpr ⟨List.mem_finRange row, accepted⟩, keyAtRow⟩

theorem Grouping.observed_group_nonempty (partition : Grouping domain key) [DecidableEq key]
    (identity : key) (observed : identity ∈ partition.keys) :
    (partition.group identity).rows ≠ [] := by
  obtain ⟨row, member⟩ := (partition.observed_keys identity).mp observed
  intro empty
  rw [empty] at member
  exact List.not_mem_nil member

-- A grouped summary visits each observed key once. Its length is the number
-- of nonempty groups, including a group containing missing keys under keep.
def Grouping.summarize (partition : Grouping domain key) [DecidableEq key]
    (evaluate : (identity : key) → Group domain → Outcome α) :
    Outcome (Vector (key × α) partition.keys.length) :=
  Vector.ofFnM fun position => do
    let identity := partition.keys[position.val]
    let value ← evaluate identity (partition.group identity)
    pure (identity, value)

def distinctCount [DecidableEq α] (policy : MissingPolicy) (cells : List (Option α)) :
    Outcome RInt := do
  let domain : Domain := ⟨0, cells.length⟩
  let grouping ← Grouping.start (domain := domain) (fun row => cells[row.val])
    Option.isNone policy
  checkedInt (Int.ofNat grouping.keys.length)

end Rupam
