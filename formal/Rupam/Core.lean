import Init.Data.List.Perm

namespace Rupam

structure Domain where
  identity : Nat
  size : Nat
  deriving DecidableEq

abbrev Row (d : Domain) := Fin d.size

inductive Shape where
  | scalar
  | vector (size : Nat)
  | column (domain : Domain)
  deriving DecidableEq

abbrev Position : Shape → Type
  | .scalar => Unit
  | .vector size => Fin size
  | .column d => Row d

inductive Align : Shape → Shape → Shape → Type where
  | scalar : Align .scalar .scalar .scalar
  | vectorLeft : Align .scalar (.vector n) (.vector n)
  | vectorRight : Align (.vector n) .scalar (.vector n)
  | vectorSame : Align (.vector n) (.vector n) (.vector n)
  | left : Align .scalar (.column d) (.column d)
  | right : Align (.column d) .scalar (.column d)
  | same : Align (.column d) (.column d) (.column d)

def Align.positions : Align a b c → Position c → Position a × Position b
  | .scalar, _ => ((), ())
  | .vectorLeft, index => ((), index)
  | .vectorRight, index => (index, ())
  | .vectorSame, index => (index, index)
  | .left, row => ((), row)
  | .right, row => (row, ())
  | .same, row => (row, row)

def Align.pointwise : (shape : Shape) → Align shape shape shape
  | .scalar => .scalar
  | .vector _ => .vectorSame
  | .column _ => .same

theorem Align.pointwise_positions (position : Position shape) :
    (Align.pointwise shape).positions position = (position, position) := by
  cases shape <;> cases position <;> rfl

theorem Align.columns_same_domain (alignment : Align (.column a) (.column b) c) :
    a = b ∧ c = .column a := by
  cases alignment
  exact ⟨rfl, rfl⟩

theorem Align.vectors_same_size (alignment : Align (.vector m) (.vector n) result) :
    m = n ∧ result = .vector m := by
  cases alignment
  exact ⟨rfl, rfl⟩

theorem Align.vector_column_impossible : Align (.vector n) (.column d) result → False := by
  intro alignment
  cases alignment

theorem Align.column_vector_impossible : Align (.column d) (.vector n) result → False := by
  intro alignment
  cases alignment

-- The result carries its alignment derivation. Equal sizes suffice for vectors;
-- columns also have to name the same observation domain.
def Align.infer : (left right : Shape) → Option (Σ result, Align left right result)
  | .scalar, .scalar => some ⟨.scalar, .scalar⟩
  | .scalar, .vector n => some ⟨.vector n, .vectorLeft⟩
  | .vector n, .scalar => some ⟨.vector n, .vectorRight⟩
  | .scalar, .column d => some ⟨.column d, .left⟩
  | .column d, .scalar => some ⟨.column d, .right⟩
  | .vector m, .vector n =>
    if sameSize : m = n then by
      subst n
      exact some ⟨.vector m, .vectorSame⟩
    else none
  | .column a, .column b =>
    if sameDomain : a = b then by
      subst b
      exact some ⟨.column a, .same⟩
    else none
  | .vector _, .column _ | .column _, .vector _ => none

theorem Align.infer_complete (alignment : Align a b result) :
    Align.infer a b = some ⟨result, alignment⟩ := by
  cases alignment <;> simp [Align.infer]

theorem Align.infer_rejects_iff (left right : Shape) :
    Align.infer left right = none ↔ ¬ Nonempty (Σ result, Align left right result) := by
  constructor
  · intro rejected ⟨⟨_, alignment⟩⟩
    have accepted := alignment.infer_complete
    rw [rejected] at accepted
    cases accepted
  · intro noAlignment
    cases accepted : Align.infer left right with
    | none => rfl
    | some witness => exact False.elim (noAlignment ⟨witness⟩)

inductive Failure where
  | arithmeticOverflow
  | missingValue
  | vectorLengthMismatch
  deriving DecidableEq, Repr

abbrev Outcome := Except Failure

def intMax : Int := 9223372036854775807
def Portable (n : Int) : Prop := -intMax ≤ n ∧ n ≤ intMax
instance (n : Int) : Decidable (Portable n) := inferInstanceAs (Decidable (_ ∧ _))

abbrev RInt := { n : Int // Portable n }

def checkedInt (n : Int) : Outcome RInt :=
  if h : Portable n then .ok ⟨n, h⟩ else .error .arithmeticOverflow

theorem checkedInt_ok (n : Int) (h : Portable n) :
    checkedInt n = .ok ⟨n, h⟩ := by simp [checkedInt, h]

theorem checkedInt_overflow (n : Int) (h : ¬ Portable n) :
    checkedInt n = .error .arithmeticOverflow := by simp [checkedInt, h]

def addInt (x y : RInt) : Outcome RInt := checkedInt (x.val + y.val)

def multiplyInt (x y : RInt) : Outcome RInt := checkedInt (x.val * y.val)

def lift₂ (f : α → β → Outcome γ) (a : Option α) (b : Option β) :
    Outcome (Option γ) :=
  match a, b with
  | some x, some y => some <$> f x y
  | none, _ | _, none => .ok none

def choose (condition : Bool) (yes no : Unit → Outcome α) : Outcome α :=
  if condition then yes () else no ()

def coalesceWith (left : Outcome (Option α)) (right : Unit → Outcome β)
    (present : α → β) : Outcome β := do
  match ← left with
  | some value => pure (present value)
  | none => right ()

def coalesce (left : Outcome (Option α)) (right : Unit → Outcome α) : Outcome α :=
  coalesceWith left right id

def combineTruth (decisive : Bool) (left right : Option Bool) : Option Bool :=
  match left with
  | some value => if value == decisive then some decisive else right
  | none => if right == some decisive then some decisive else none

def guardedBoolean (decisive : Bool) (left : Option Bool) (right : Unit → Outcome (Option Bool)) :
    Outcome (Option Bool) := do
  if left == some decisive then pure (some decisive)
  else pure (combineTruth decisive left (← right ()))

def and3 := guardedBoolean false
def or3 := guardedBoolean true

def not3 (value : Option Bool) : Option Bool := value.map (! ·)

@[simp] theorem choose_true (yes no : Unit → Outcome α) :
    choose true yes no = yes () := rfl

@[simp] theorem choose_false (yes no : Unit → Outcome α) :
    choose false yes no = no () := rfl

@[simp] theorem coalesce_present (value : α) (right : Unit → Outcome α) :
    coalesce (.ok (some value)) right = .ok value := rfl

@[simp] theorem and3_false (right : Unit → Outcome (Option Bool)) :
    and3 (some false) right = .ok (some false) := rfl

@[simp] theorem or3_true (right : Unit → Outcome (Option Bool)) :
    or3 (some true) right = .ok (some true) := rfl

inductive Base where
  | int
  | bool
  deriving DecidableEq

abbrev Base.denote : Base → Type
  | .int => RInt
  | .bool => Bool

inductive Ty where
  | required (base : Base)
  | optional (base : Base)
  | vector (size : Nat) (element : Ty)
  | column (domain : Domain) (element : Ty)
  | function (input output : Ty)
  deriving DecidableEq

abbrev Ty.denote : Ty → Type
  | .required base => base.denote
  | .optional base => Option base.denote
  | .vector size element => Vector element.denote size
  | .column domain element => Row domain → element.denote
  | .function input output => input.denote → Outcome output.denote

def Shape.valueType (shape : Shape) (cell : Ty) : Ty :=
  match shape with
  | .scalar => cell
  | .vector n => .vector n cell
  | .column d => .column d cell

-- A fragment of checked Core. Inputs and scalar broadcasts are already resolved.
inductive Expr : Shape → Ty → Type where
  | literal : t.denote → Expr .scalar t
  | vector : Vector t.denote n → Expr (.vector n) t
  | column : (Row d → t.denote) → Expr (.column d) t
  | broadcastVector : Expr .scalar t → Expr (.vector n) t
  | broadcast : Expr .scalar t → Expr (.column d) t
  | optional : Expr s (.required base) → Expr s (.optional base)
  | add : Align a b c → Expr a (.required .int) → Expr b (.required .int) →
      Expr c (.required .int)
  | multiply : Align a b c → Expr a (.required .int) → Expr b (.required .int) →
      Expr c (.required .int)
  | addOptional : Align a b c → Expr a (.optional .int) → Expr b (.optional .int) →
      Expr c (.optional .int)
  | multiplyOptional : Align a b c → Expr a (.optional .int) → Expr b (.optional .int) →
      Expr c (.optional .int)
  | apply : Expr s (.function input output) → Expr s input → Expr s output
  | ifThenElse : Expr s (.required .bool) → Expr s t → Expr s t → Expr s t
  | coalesce : Expr s (.optional base) → Expr s (.required base) → Expr s (.required base)
  | coalesceOptional : Expr s (.optional base) → Expr s (.optional base) →
      Expr s (.optional base)
  | and : Expr s (.optional .bool) → Expr s (.optional .bool) → Expr s (.optional .bool)
  | or : Expr s (.optional .bool) → Expr s (.optional .bool) → Expr s (.optional .bool)
  | not : Expr s (.optional .bool) → Expr s (.optional .bool)

def Expr.eval : Expr s t → Position s → Outcome t.denote
  | .literal value, _ => .ok value
  | .vector values, index => .ok (values.get index)
  | .column get, row => .ok (get row)
  | .broadcastVector value, _ => value.eval ()
  | .broadcast value, _ => value.eval ()
  | .optional value, pos => some <$> value.eval pos
  | .add align left right, pos => do
    let (l, r) := align.positions pos
    addInt (← left.eval l) (← right.eval r)
  | .multiply align left right, pos => do
    let (l, r) := align.positions pos
    multiplyInt (← left.eval l) (← right.eval r)
  | .addOptional align left right, pos => do
    let (l, r) := align.positions pos
    lift₂ addInt (← left.eval l) (← right.eval r)
  | .multiplyOptional align left right, pos => do
    let (l, r) := align.positions pos
    lift₂ multiplyInt (← left.eval l) (← right.eval r)
  | .apply function argument, pos => do
    let invoke ← function.eval pos
    invoke (← argument.eval pos)
  | .ifThenElse condition yes no, pos => do
    choose (← condition.eval pos) (fun _ => yes.eval pos) (fun _ => no.eval pos)
  | .coalesce left right, pos => Rupam.coalesce (left.eval pos) (fun _ => right.eval pos)
  | .coalesceOptional left right, pos =>
    coalesceWith (left.eval pos) (fun _ => right.eval pos) some
  | .and left right, pos => do and3 (← left.eval pos) (fun _ => right.eval pos)
  | .or left right, pos => do or3 (← left.eval pos) (fun _ => right.eval pos)
  | .not value, pos => not3 <$> value.eval pos

def Expr.broadcastTo (value : Expr .scalar t) : (shape : Shape) → Expr shape t
  | .scalar => value
  | .vector _ => .broadcastVector value
  | .column _ => .broadcast value

theorem Expr.eval_broadcastTo (value : Expr .scalar t) (position : Position shape) :
    (value.broadcastTo shape).eval position = value.eval () := by
  cases shape with
  | scalar => cases position; rfl
  | vector _ => rfl
  | column _ => rfl

theorem Expr.eval_if_true (condition : Expr s (.required .bool)) (yes no : Expr s t)
    (pos : Position s) (selected : condition.eval pos = .ok true) :
    (Expr.ifThenElse condition yes no).eval pos = yes.eval pos := by
  -- Once the condition succeeds, only the selected thunk is forced.
  rw [Expr.eval, selected]
  rfl

theorem Expr.eval_if_false (condition : Expr s (.required .bool)) (yes no : Expr s t)
    (pos : Position s) (selected : condition.eval pos = .ok false) :
    (Expr.ifThenElse condition yes no).eval pos = no.eval pos := by
  rw [Expr.eval, selected]
  rfl

inductive MissingPolicy where
  | skip
  | keep
  | error
  deriving DecidableEq

def firstWithPolicy (policy : MissingPolicy) (cells : List (Option α)) : Outcome (Option α) :=
  match policy with
  | .skip => .ok (cells.findSome? id)
  | .keep => .ok cells.head?.join
  | .error =>
    if cells.any Option.isNone then .error .missingValue
    else .ok cells.head?.join

def lastWithPolicy (policy : MissingPolicy) (cells : List (Option α)) : Outcome (Option α) :=
  firstWithPolicy policy cells.reverse

def sumInt (policy : MissingPolicy) (cells : List (Option RInt)) :
    Outcome (Option RInt) := do
  let present := cells.filterMap id
  let hasMissing := present.length != cells.length
  match policy, hasMissing with
  | .keep, true => pure none
  | .error, true => .error .missingValue
  | _, _ =>
    if present.isEmpty then pure none
    else some <$> checkedInt (present.map Subtype.val).sum

private theorem sum_eq_of_perm {xs ys : List Int} (perm : xs.Perm ys) :
    xs.sum = ys.sum := by
  induction perm with
  | nil =>
    rfl
  | cons value _ tailSum =>
    -- A shared head contributes the same amount to both sums.
    simp only [List.sum_cons, tailSum]
  | swap x y tail =>
    -- Swapping adjacent cells keeps the tail and exchanges the summands.
    change y + (x + tail.sum) = x + (y + tail.sum)
    exact Int.add_left_comm y x tail.sum
  | trans _ _ first second =>
    exact first.trans second

theorem sumInt_perm (policy : MissingPolicy) (perm : cells.Perm other) :
    sumInt policy cells = sumInt policy other := by
  -- Removing missing cells leaves a permutation of the present values.
  have presentPerm := perm.filterMap id
  have sameCellCount := perm.length_eq
  have samePresentCount := presentPerm.length_eq
  have sameEmptiness := presentPerm.isEmpty_eq
  have sameTotal := sum_eq_of_perm (presentPerm.map Subtype.val)
  -- These facts determine the missing-policy decision and the final range check.
  simp only [sumInt, sameCellCount, samePresentCount, sameEmptiness, sameTotal]

structure Group (d : Domain) where
  rows : List (Row d)
  unique : rows.Nodup

-- The mask selects consumers. A demanded reduction still sees every group row.
def guardedReduction (group : Group d) (select : Row d → Bool)
    (reduce : List (Row d) → Outcome α) : Outcome (List (Row d × α)) := do
  match group.rows.filter select with
  | [] => pure []
  | row :: rest =>
    let value ← reduce group.rows
    pure ((row :: rest).map (·, value))

theorem guardedReduction_inactive (group : Group d) (select : Row d → Bool)
    (reduce : List (Row d) → Outcome α) (inactive : group.rows.filter select = []) :
    guardedReduction group select reduce = .ok [] := by
  -- The empty selection returns before the reduction is demanded.
  unfold guardedReduction
  rw [inactive]
  rfl

theorem guardedReduction_active (group : Group d) (select : Row d → Bool)
    (reduce : List (Row d) → Outcome α) (active : group.rows.filter select = row :: rest) :
    guardedReduction group select reduce =
      (do
        let value ← reduce group.rows
        pure ((row :: rest).map (·, value))) := by
  unfold guardedReduction
  rw [active]

end Rupam
