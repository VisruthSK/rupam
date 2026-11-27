import Rupam.Core

namespace Rupam

inductive AlignArguments : List Shape → Shape → Type where
  | nil : AlignArguments [] .scalar
  | cons : Align input tail result → AlignArguments rest tail →
      AlignArguments (input :: rest) result

def AlignArguments.infer : (inputs : List Shape) → Option (Σ result, AlignArguments inputs result)
  | [] => some ⟨.scalar, .nil⟩
  | input :: rest => do
    let ⟨tailShape, tail⟩ ← infer rest
    let ⟨resultShape, head⟩ ← Align.infer input tailShape
    pure ⟨resultShape, .cons head tail⟩

theorem AlignArguments.infer_complete (alignment : AlignArguments inputs result) :
    infer inputs = some ⟨result, alignment⟩ := by
  induction alignment with
  | nil => rfl
  | @cons input tailShape result rest head tail tailComplete =>
    rw [infer, tailComplete]
    change (do
      let checked ← Align.infer input tailShape
      pure (⟨checked.1, .cons checked.2 tail⟩ : Σ shape, AlignArguments (input :: rest) shape)) = _
    rw [head.infer_complete]
    rfl

theorem AlignArguments.infer_rejects_iff (inputs : List Shape) :
    infer inputs = none ↔ ¬ Nonempty (Σ result, AlignArguments inputs result) := by
  constructor
  · intro rejected ⟨⟨_, alignment⟩⟩
    have accepted := alignment.infer_complete
    rw [rejected] at accepted
    cases accepted
  · intro impossible
    cases checked : infer inputs with
    | none => rfl
    | some witness => exact False.elim (impossible ⟨witness⟩)

theorem AlignArguments.result_unique (first : AlignArguments inputs a)
    (second : AlignArguments inputs b) : a = b := by
  have checked := first.infer_complete.symm.trans second.infer_complete
  exact congrArg Sigma.fst (Option.some.inj checked)

inductive ArgumentShape : List Shape → Shape → Type where
  | first : ArgumentShape (shape :: rest) shape
  | next : ArgumentShape rest shape → ArgumentShape (other :: rest) shape

theorem ArgumentShape.of_mem (member : input ∈ inputs) : Nonempty (ArgumentShape inputs input) := by
  induction inputs with
  | nil => cases member
  | cons head tail tailShape =>
    rcases List.mem_cons.mp member with rfl | inTail
    · exact ⟨.first⟩
    · let ⟨position⟩ := tailShape inTail
      exact ⟨.next position⟩

def AlignArguments.position (alignment : AlignArguments inputs result)
    (position : Position result) : ArgumentShape inputs input → Position input :=
  match alignment with
  | .nil => fun argument => nomatch argument
  | .cons head tail =>
    let (first, rest) := head.positions position
    fun
    | .first => first
    | .next argument => tail.position rest argument

private theorem Align.left_column (alignment : Align (.column d) input result) :
    result = .column d := by
  cases alignment <;> rfl

private theorem Align.right_column (alignment : Align input (.column d) result) :
    result = .column d := by
  cases alignment <;> rfl

theorem AlignArguments.column_forces_shape (alignment : AlignArguments inputs result)
    (argument : ArgumentShape inputs (.column d)) : result = .column d := by
  induction alignment with
  | nil => nomatch argument
  | cons head tail tailShape =>
    cases argument with
    | first => exact head.left_column
    | next rest =>
      have columnTail := tailShape rest
      rw [columnTail] at head
      exact head.right_column

private theorem Align.left_vector (alignment : Align (.vector n) input result) :
    result = .vector n := by
  cases alignment <;> rfl

private theorem Align.right_vector (alignment : Align input (.vector n) result) :
    result = .vector n := by
  cases alignment <;> rfl

theorem AlignArguments.vector_forces_shape (alignment : AlignArguments inputs result)
    (argument : ArgumentShape inputs (.vector n)) : result = .vector n := by
  induction alignment with
  | nil => nomatch argument
  | cons head tail tailShape =>
    cases argument with
    | first => exact head.left_vector
    | next rest =>
      have vectorTail := tailShape rest
      rw [vectorTail] at head
      exact head.right_vector

theorem AlignArguments.columns_same_domain (alignment : AlignArguments inputs result)
    (first : ArgumentShape inputs (.column a)) (second : ArgumentShape inputs (.column b)) :
    a = b :=
  Shape.column.inj ((alignment.column_forces_shape first).symm.trans
    (alignment.column_forces_shape second))

theorem AlignArguments.vectors_same_size (alignment : AlignArguments inputs result)
    (first : ArgumentShape inputs (.vector m)) (second : ArgumentShape inputs (.vector n)) :
    m = n :=
  Shape.vector.inj ((alignment.vector_forces_shape first).symm.trans
    (alignment.vector_forces_shape second))

theorem AlignArguments.no_column_vector (alignment : AlignArguments inputs result)
    (column : ArgumentShape inputs (.column d)) (vector : ArgumentShape inputs (.vector n)) :
    False := by
  have impossible := (alignment.column_forces_shape column).symm.trans
    (alignment.vector_forces_shape vector)
  cases impossible

theorem AlignArguments.vector_alignment_iff
    (scalarOrVector : ∀ shape ∈ inputs, shape = .scalar ∨ ∃ n, shape = .vector n) :
    Nonempty (Σ result, AlignArguments inputs result) ↔
      ∃ size, ∀ shape ∈ inputs, shape = .scalar ∨ shape = .vector size := by
  constructor
  · intro ⟨⟨result, alignment⟩⟩
    let size := match result with
      | .vector n => n
      | .scalar | .column _ => 0
    refine ⟨size, ?_⟩
    intro shape member
    rcases scalarOrVector shape member with scalar | ⟨n, rfl⟩
    · exact Or.inl scalar
    · obtain ⟨argument⟩ := ArgumentShape.of_mem member
      have same := alignment.vector_forces_shape argument
      exact Or.inr (by simp [size, same])
  · intro ⟨size, compatible⟩
    suffices ∃ result, (result = .scalar ∨ result = .vector size) ∧
        Nonempty (AlignArguments inputs result) by
      obtain ⟨result, _, ⟨alignment⟩⟩ := this
      exact ⟨⟨result, alignment⟩⟩
    clear scalarOrVector
    induction inputs with
    | nil => exact ⟨.scalar, Or.inl rfl, ⟨.nil⟩⟩
    | cons input rest tailAligned =>
      obtain ⟨result, tailShape, ⟨tail⟩⟩ := tailAligned
        (fun shape member => compatible shape (List.mem_cons_of_mem _ member))
      rcases compatible input List.mem_cons_self with rfl | rfl <;>
        rcases tailShape with rfl | rfl
      · exact ⟨.scalar, Or.inl rfl, ⟨.cons .scalar tail⟩⟩
      · exact ⟨.vector size, Or.inr rfl, ⟨.cons .vectorLeft tail⟩⟩
      · exact ⟨.vector size, Or.inr rfl, ⟨.cons .vectorRight tail⟩⟩
      · exact ⟨.vector size, Or.inr rfl, ⟨.cons .vectorSame tail⟩⟩

inductive Broadcasts : Shape → Shape → Type where
  | same : Broadcasts shape shape
  | vector : Broadcasts .scalar (.vector n)
  | column : Broadcasts .scalar (.column d)

def Broadcasts.core : Broadcasts source target → Expr source t → Expr target t
  | .same, expression => expression
  | .vector, expression => .broadcastVector expression
  | .column, expression => .broadcast expression

def Broadcasts.trans : Broadcasts a b → Broadcasts b c → Broadcasts a c
  | .same, later => later
  | .vector, .same => .vector
  | .column, .same => .column

def Align.leftBroadcast (alignment : Align a b result) : Broadcasts a result := by
  cases alignment <;> first | exact .same | exact .vector | exact .column

def Align.rightBroadcast (alignment : Align a b result) : Broadcasts b result := by
  cases alignment <;> first | exact .same | exact .vector | exact .column

theorem Align.eval_leftBroadcast (alignment : Align a b result) (expression : Expr a t)
    (position : Position result) :
    (alignment.leftBroadcast.core expression).eval position =
      expression.eval (alignment.positions position).1 := by
  cases alignment <;> first | rfl | cases position; rfl

theorem Align.eval_rightBroadcast (alignment : Align a b result) (expression : Expr b t)
    (position : Position result) :
    (alignment.rightBroadcast.core expression).eval position =
      expression.eval (alignment.positions position).2 := by
  cases alignment <;> first | rfl | cases position; rfl

def AlignArguments.broadcasts (alignment : AlignArguments inputs result) :
    ArgumentShape inputs input → Broadcasts input result :=
  match alignment with
  | .nil => fun argument => nomatch argument
  | .cons head tail => fun
    | .first => head.leftBroadcast
    | .next argument => (tail.broadcasts argument).trans head.rightBroadcast

end Rupam
