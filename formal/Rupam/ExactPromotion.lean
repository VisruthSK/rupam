import Rupam.Core
import Init.Data.List.Find
import Init.Data.List.FinRange

namespace Rupam

-- The sign is inherited from the integer. A portable integer fits exactly
-- when its magnitude has a 53-bit significand times a power of two.
structure IntegerEncoding (n : RInt) where
  shift : Fin 64
  significand : Nat
  fits : significand < 2 ^ 53
  exactMagnitude : n.val.natAbs = significand * 2 ^ shift.val

private theorem portable_magnitude (n : RInt) : n.val.natAbs < 2 ^ 63 := by
  obtain ⟨value, portable⟩ := n
  unfold Portable intMax at portable
  cases value with
  | ofNat magnitude =>
    change -9223372036854775807 ≤ (magnitude : Int) ∧
      (magnitude : Int) ≤ 9223372036854775807 at portable
    change magnitude < 9223372036854775808
    omega
  | negSucc magnitude =>
    change magnitude + 1 < 9223372036854775808
    omega

theorem integer_encoding_iff (n : RInt) :
    Nonempty (IntegerEncoding n) ↔
      ∃ significand shift : Nat, significand < 2 ^ 53 ∧ n.val.natAbs = significand * 2 ^ shift := by
  constructor
  · intro ⟨encoding⟩
    exact ⟨encoding.significand, encoding.shift.val, encoding.fits, encoding.exactMagnitude⟩
  · intro ⟨significand, shift, fits, exactMagnitude⟩
    by_cases zero : significand = 0
    · refine ⟨⟨0, 0, by decide, ?_⟩⟩
      rw [zero, Nat.zero_mul] at exactMagnitude
      exact exactMagnitude
    · have nonzero : 0 < significand := Nat.pos_of_ne_zero zero
      have bounded : shift < 64 := by
        by_cases small : shift < 64
        · exact small
        · have power : 2 ^ 64 ≤ 2 ^ shift := Nat.pow_le_pow_right (by decide) (by omega)
          have magnitude : 2 ^ shift ≤ n.val.natAbs := by
            rw [exactMagnitude]
            exact Nat.le_mul_of_pos_left _ nonzero
          have tooLarge := Nat.le_trans power magnitude
          have portable := portable_magnitude n
          change 18446744073709551616 ≤ n.val.natAbs at tooLarge
          change n.val.natAbs < 9223372036854775808 at portable
          omega
      exact ⟨⟨⟨shift, bounded⟩, significand, fits, exactMagnitude⟩⟩

private def encodingAt (n : RInt) (shift : Fin 64) : Option (IntegerEncoding n) :=
  let significand := n.val.natAbs / 2 ^ shift.val
  if valid : significand < 2 ^ 53 ∧ n.val.natAbs = significand * 2 ^ shift.val then
    some ⟨shift, significand, valid.1, valid.2⟩
  else none

def encodeIntegerExactly (n : RInt) : Option (IntegerEncoding n) :=
  (List.finRange 64).findSome? (encodingAt n)

private theorem encodingAt_complete (encoding : IntegerEncoding n) :
    ∃ checked, encodingAt n encoding.shift = some checked := by
  have quotient : n.val.natAbs / 2 ^ encoding.shift.val = encoding.significand := by
    rw [encoding.exactMagnitude]
    exact Nat.mul_div_cancel encoding.significand (Nat.two_pow_pos _)
  have valid : n.val.natAbs / 2 ^ encoding.shift.val < 2 ^ 53 ∧
      n.val.natAbs = n.val.natAbs / 2 ^ encoding.shift.val * 2 ^ encoding.shift.val := by
    rw [quotient]
    exact ⟨encoding.fits, encoding.exactMagnitude⟩
  exact ⟨_, by
    unfold encodingAt
    rw [dite_eq_left valid]⟩

theorem exact_encoding_exists_iff (n : RInt) :
    (∃ encoding, encodeIntegerExactly n = some encoding) ↔ Nonempty (IntegerEncoding n) := by
  constructor
  · intro ⟨encoding, _⟩
    exact ⟨encoding⟩
  · intro ⟨encoding⟩
    cases checked : encodeIntegerExactly n with
    | some result => exact ⟨result, rfl⟩
    | none =>
      obtain ⟨found, accepted⟩ := encodingAt_complete encoding
      unfold encodeIntegerExactly at checked
      have missing := List.findSome?_eq_none_iff.mp checked encoding.shift (List.mem_finRange _)
      rw [accepted] at missing
      cases missing

theorem rejects_inexact_integer (n : RInt) :
    encodeIntegerExactly n = none ↔ ¬ Nonempty (IntegerEncoding n) := by
  constructor
  · intro rejected possible
    obtain ⟨result, accepted⟩ := (exact_encoding_exists_iff n).mpr possible
    rw [rejected] at accepted
    cases accepted
  · intro impossible
    cases checked : encodeIntegerExactly n with
    | none => rfl
    | some encoding => exact False.elim (impossible ⟨encoding⟩)

end Rupam
