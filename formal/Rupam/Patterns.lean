import Rupam.MatchExecution

namespace Rupam

inductive Pattern : Type → Type → Type 1 where
  | wildcard : Pattern α Unit
  | binding : Pattern α α
  | predicate (accepts : α → Bool) : Pattern α Unit
  | absent : Pattern (Option α) Unit
  | present : Pattern α bindings → Pattern (Option α) bindings
  | product : Pattern α left → Pattern β right → Pattern (α × β) (left × right)

def Pattern.literal [DecidableEq α] (value : α) : Pattern α Unit :=
  .predicate (fun subject => decide (subject = value))

def Pattern.matchValue : Pattern α bindings → α → Option bindings
  | .wildcard, _ => some ()
  | .binding, value => some value
  | .predicate accepts, value => if accepts value then some () else none
  | .absent, none => some ()
  | .absent, some _ => none
  | .present pattern, value => value.bind pattern.matchValue
  | .product left right, (a, b) => do
    let x ← left.matchValue a
    let y ← right.matchValue b
    pure (x, y)

inductive Pattern.Matches : {α bindings : Type} → Pattern α bindings → α → bindings → Prop where
  | wildcard : Matches (.wildcard : Pattern α Unit) value ()
  | binding : Matches (.binding : Pattern α α) value value
  | predicate {accepts : α → Bool} {value : α} (accepted : accepts value = true) :
      Matches (.predicate accepts) value ()
  | absent : Matches (.absent : Pattern (Option α) Unit) none ()
  | present : Matches pattern value bindings → Matches (.present pattern) (some value) bindings
  | product : Matches left a x → Matches right b y → Matches (.product left right) (a, b) (x, y)

theorem Pattern.Matches.evaluates {pattern : Pattern α bindings} (matchProof : pattern.Matches subject bound) :
    pattern.matchValue subject = some bound := by
  induction matchProof with
  | wildcard => rfl
  | binding => rfl
  | predicate accepted => simp [matchValue, accepted]
  | absent => rfl
  | present inner read => simpa [matchValue] using read
  | product left right readLeft readRight =>
    simp [matchValue, readLeft, readRight] <;> rfl

theorem Pattern.matchValue_sound (pattern : Pattern α bindings) (subject : α) (bound : bindings)
    (matched : pattern.matchValue subject = some bound) : pattern.Matches subject bound := by
  induction pattern with
  | wildcard =>
    cases matched
    exact .wildcard
  | binding =>
    cases matched
    exact .binding
  | predicate accepts =>
    by_cases passed : accepts subject = true
    · cases bound
      exact .predicate passed
    · simp [matchValue, passed] at matched
  | absent =>
    cases subject with
    | none => cases bound; exact .absent
    | some value => cases matched
  | present inner sound =>
    cases subject with
    | none => cases matched
    | some value => exact .present (sound value bound matched)
  | product left right soundLeft soundRight =>
    obtain ⟨a, b⟩ := subject
    unfold matchValue at matched
    cases readLeft : left.matchValue a with
    | none => rw [readLeft] at matched; cases matched
    | some x =>
      rw [readLeft] at matched
      simp only [bind, Option.bind] at matched
      cases readRight : right.matchValue b with
      | none => rw [readRight] at matched; cases matched
      | some y =>
        rw [readRight] at matched
        have same : (x, y) = bound := Option.some.inj matched
        subst bound
        exact .product (soundLeft _ _ readLeft) (soundRight _ _ readRight)

theorem Pattern.literal_matches [DecidableEq α] (expected subject : α) :
    (Pattern.literal expected).Matches subject () ↔ subject = expected := by
  constructor
  · intro matched
    have evaluated := matched.evaluates
    by_cases same : subject = expected
    · exact same
    · simp [literal, matchValue, same] at evaluated
  · intro same
    exact .predicate (decide_eq_true same)

def Pattern.testGuard (pattern : Pattern α bindings) (guard : bindings → Outcome Bool) (subject : α) :
    Outcome Bool :=
  match pattern.matchValue subject with
  | some values => guard values
  | none => .ok false

theorem Pattern.guard_acceptance (pattern : Pattern α bindings) (guard : bindings → Outcome Bool)
    (subject : α) :
    pattern.testGuard guard subject = .ok true ↔
      ∃ values, pattern.Matches subject values ∧ guard values = .ok true := by
  constructor
  · intro passed
    unfold testGuard at passed
    cases matched : pattern.matchValue subject with
    | none =>
      rw [matched] at passed
      cases passed
    | some values =>
      rw [matched] at passed
      exact ⟨values, pattern.matchValue_sound subject values matched, passed⟩
  · intro ⟨values, matched, passed⟩
    unfold testGuard
    rw [matched.evaluates]
    exact passed

end Rupam
