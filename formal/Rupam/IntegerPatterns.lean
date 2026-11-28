import Rupam.Patterns

namespace Rupam

inductive IntegerEndpoint where
  | unbounded
  | inclusive (endpoint : Int)
  | exclusive (endpoint : Int)

def IntegerEndpoint.allowsAbove : IntegerEndpoint → Int → Prop
  | .unbounded, _ => True
  | .inclusive lower, value => lower ≤ value
  | .exclusive lower, value => lower < value

def IntegerEndpoint.allowsBelow : IntegerEndpoint → Int → Prop
  | .unbounded, _ => True
  | .inclusive upper, value => value ≤ upper
  | .exclusive upper, value => value < upper

def IntegerEndpoint.lowerLimit : IntegerEndpoint → Int
  | .unbounded => -intMax
  | .inclusive lower => max (-intMax) lower
  | .exclusive lower => max (-intMax) (lower + 1)

def IntegerEndpoint.upperLimit : IntegerEndpoint → Int
  | .unbounded => intMax
  | .inclusive upper => min intMax upper
  | .exclusive upper => min intMax (upper - 1)

theorem IntegerEndpoint.lower_limit_correct (endpoint : IntegerEndpoint) (value : RInt) :
    endpoint.lowerLimit ≤ value.val ↔ endpoint.allowsAbove value.val := by
  have portable := value.property.1
  cases endpoint with
  | unbounded => simp [lowerLimit, allowsAbove, portable]
  | inclusive lower => simp [lowerLimit, allowsAbove, Int.max_le, portable]
  | exclusive lower =>
    simp only [lowerLimit, allowsAbove, Int.max_le, portable, true_and]
    omega

theorem IntegerEndpoint.upper_limit_correct (endpoint : IntegerEndpoint) (value : RInt) :
    value.val ≤ endpoint.upperLimit ↔ endpoint.allowsBelow value.val := by
  have portable := value.property.2
  cases endpoint with
  | unbounded => simp [upperLimit, allowsBelow, portable]
  | inclusive upper => simp [upperLimit, allowsBelow, Int.le_min, portable]
  | exclusive upper =>
    simp only [upperLimit, allowsBelow, Int.le_min, portable, true_and]
    omega

private theorem IntegerEndpoint.lower_is_portable (endpoint : IntegerEndpoint) :
    -intMax ≤ endpoint.lowerLimit := by
  cases endpoint with
  | unbounded => exact Int.le_refl _
  | inclusive _ => exact Int.le_max_left _ _
  | exclusive _ => exact Int.le_max_left _ _

private theorem IntegerEndpoint.upper_is_portable (endpoint : IntegerEndpoint) :
    endpoint.upperLimit ≤ intMax := by
  cases endpoint with
  | unbounded => exact Int.le_refl _
  | inclusive _ => exact Int.min_le_left _ _
  | exclusive _ => exact Int.min_le_left _ _

structure IntegerRange where
  lower : IntegerEndpoint
  upper : IntegerEndpoint

def IntegerRange.Covers (range : IntegerRange) (value : RInt) : Prop :=
  range.lower.allowsAbove value.val ∧ range.upper.allowsBelow value.val

def IntegerRange.overlaps (first second : IntegerRange) : Bool :=
  decide (max first.lower.lowerLimit second.lower.lowerLimit ≤ min first.upper.upperLimit second.upper.upperLimit)

theorem IntegerRange.overlaps_iff (first second : IntegerRange) :
    first.overlaps second = true ↔ ∃ value, first.Covers value ∧ second.Covers value := by
  constructor
  · intro checked
    let point := max first.lower.lowerLimit second.lower.lowerLimit
    have fits : point ≤ min first.upper.upperLimit second.upper.upperLimit :=
      of_decide_eq_true checked
    have firstUpper := Int.le_trans fits (Int.min_le_left first.upper.upperLimit second.upper.upperLimit)
    have secondUpper := Int.le_trans fits (Int.min_le_right first.upper.upperLimit second.upper.upperLimit)
    have firstLower := Int.le_max_left first.lower.lowerLimit second.lower.lowerLimit
    have secondLower := Int.le_max_right first.lower.lowerLimit second.lower.lowerLimit
    let value : RInt := ⟨point, Int.le_trans first.lower.lower_is_portable firstLower,
      Int.le_trans firstUpper first.upper.upper_is_portable⟩
    exact ⟨value,
      ⟨(first.lower.lower_limit_correct value).mp firstLower,
        (first.upper.upper_limit_correct value).mp firstUpper⟩,
      ⟨(second.lower.lower_limit_correct value).mp secondLower,
        (second.upper.upper_limit_correct value).mp secondUpper⟩⟩
  · intro ⟨value, inFirst, inSecond⟩
    have lower := Int.max_le.mpr ⟨(first.lower.lower_limit_correct value).mpr inFirst.1,
      (second.lower.lower_limit_correct value).mpr inSecond.1⟩
    have upper := Int.le_min.mpr ⟨(first.upper.upper_limit_correct value).mpr inFirst.2,
      (second.upper.upper_limit_correct value).mpr inSecond.2⟩
    exact decide_eq_true (Int.le_trans lower upper)

def IntegerRange.pattern (range : IntegerRange) : Pattern RInt Unit :=
  .predicate fun value => decide (range.lower.lowerLimit ≤ value.val ∧ value.val ≤ range.upper.upperLimit)

theorem IntegerRange.pattern_correct (range : IntegerRange) (value : RInt) :
    range.pattern.Matches value () ↔ range.Covers value := by
  constructor
  · intro matched
    have evaluated := matched.evaluates
    have passes : range.lower.lowerLimit ≤ value.val ∧ value.val ≤ range.upper.upperLimit := by
      by_cases bounds : range.lower.lowerLimit ≤ value.val ∧ value.val ≤ range.upper.upperLimit
      · exact bounds
      · simp [pattern, Pattern.matchValue, bounds] at evaluated
    exact ⟨(range.lower.lower_limit_correct value).mp passes.1,
      (range.upper.upper_limit_correct value).mp passes.2⟩
  · intro covers
    apply range.pattern.matchValue_sound
    have bounds := And.intro ((range.lower.lower_limit_correct value).mpr covers.1)
      ((range.upper.upper_limit_correct value).mpr covers.2)
    simp [pattern, Pattern.matchValue, bounds]

end Rupam
