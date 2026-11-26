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

end Rupam
