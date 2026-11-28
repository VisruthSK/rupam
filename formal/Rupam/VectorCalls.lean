import Rupam.Application

namespace Rupam

inductive VectorExtent where
  | known (size : Nat)
  | runtime

def VectorExtent.Values (extent : VectorExtent) (α : Type) : Type :=
  match extent with
  | .known size => Vector α size
  | .runtime => Σ size, Vector α size

private def VectorExtent.materialized (extent : VectorExtent) (values : extent.Values α) :
    Σ size, Vector α size :=
  match extent with
  | .known size => ⟨size, values⟩
  | .runtime => values

inductive VectorPassing where
  | whole
  | lifted (extent : VectorExtent)

-- A whole vector is an ordinary value here, regardless of its length.
def VectorPassing.fixedShapes : List VectorPassing → List Shape
  | [] => []
  | .whole :: rest => .scalar :: fixedShapes rest
  | .lifted (.known size) :: rest => .vector size :: fixedShapes rest
  | .lifted .runtime :: rest => fixedShapes rest

inductive VectorCallSources : List Ty → List VectorPassing → Type where
  | nil : VectorCallSources [] []
  | whole : (Unit → Outcome t.denote) → VectorCallSources rest plan →
      VectorCallSources (t :: rest) (.whole :: plan)
  | lifted (extent : VectorExtent) : (Unit → Outcome (extent.Values t.denote)) →
      VectorCallSources rest plan → VectorCallSources (t :: rest) (.lifted extent :: plan)

inductive VectorCallValues : List Ty → List VectorPassing → Type where
  | nil : VectorCallValues [] []
  | whole : t.denote → VectorCallValues rest plan → VectorCallValues (t :: rest) (.whole :: plan)
  | lifted (extent : VectorExtent) : extent.Values t.denote → VectorCallValues rest plan →
      VectorCallValues (t :: rest) (.lifted extent :: plan)

def VectorCallSources.evaluate : VectorCallSources parameters plan →
    Outcome (VectorCallValues parameters plan)
  | .nil => .ok .nil
  | .whole source rest => do pure (.whole (← source ()) (← rest.evaluate))
  | .lifted extent source rest => do pure (.lifted extent (← source ()) (← rest.evaluate))

structure MaterializedCall (parameters : List Ty) where
  actuals : List Ty
  shapes : List Shape
  passing : ArgumentPassings actuals parameters shapes
  values : ArgumentValues actuals

def VectorCallValues.materialize : VectorCallValues parameters plan → MaterializedCall parameters
  | .nil => ⟨[], [], .nil, fun p => nomatch p⟩
  | .whole value rest =>
    let tail := rest.materialize
    ⟨_, _, .cons .whole tail.passing, ArgumentValues.cons value tail.values⟩
  | .lifted extent values rest =>
    let ⟨_, cells⟩ := extent.materialized values
    let tail := rest.materialize
    ⟨_, _, .cons .vector tail.passing, ArgumentValues.cons cells tail.values⟩

structure CheckedVectorCall (plan : List VectorPassing) where
  staticShape : Shape
  staticAlignment : AlignArguments (VectorPassing.fixedShapes plan) staticShape

def CheckedVectorCall.infer (plan : List VectorPassing) : Option (CheckedVectorCall plan) :=
  (AlignArguments.infer (VectorPassing.fixedShapes plan)).map fun ⟨shape, alignment⟩ =>
    ⟨shape, alignment⟩

theorem CheckedVectorCall.infer_rejects_iff (plan : List VectorPassing) :
    infer plan = none ↔ ¬ Nonempty (CheckedVectorCall plan) := by
  constructor
  · intro rejected ⟨call⟩
    simp [infer, call.staticAlignment.infer_complete] at rejected
  · intro impossible
    cases accepted : infer plan with
    | none => rfl
    | some call => exact False.elim (impossible ⟨call⟩)

def MaterializedCall.align (inputs : MaterializedCall parameters) :
    Option (Σ shape, CheckedCall inputs.actuals parameters shape) :=
  (AlignArguments.infer inputs.shapes).map fun ⟨shape, alignment⟩ =>
    ⟨shape, ⟨inputs.shapes, inputs.passing, alignment⟩⟩

def CheckedVectorCall.run (_checked : CheckedVectorCall plan) (body : ScalarBody parameters t)
    (sources : VectorCallSources parameters plan) (demanded : (shape : Shape) → List (Position shape)) :
    Outcome (Shape × List t.denote) := do
  let evaluated ← sources.evaluate
  let inputs := evaluated.materialize
  match inputs.align with
  | none => .error .vectorLengthMismatch
  | some ⟨shape, call⟩ =>
    let values ← call.runValues body inputs.values (demanded shape)
    pure ⟨shape, values⟩

theorem CheckedVectorCall.run_preserves_pointwise (checked : CheckedVectorCall plan)
    (body : ScalarBody parameters t) (sources : VectorCallSources parameters plan)
    (demanded : (shape : Shape) → List (Position shape)) :
    checked.run body sources demanded = (do
      let evaluated ← sources.evaluate
      let inputs := evaluated.materialize
      match inputs.align with
      | none => .error .vectorLengthMismatch
      | some ⟨shape, call⟩ =>
        let values ← (demanded shape).mapM fun position =>
          body.eval (fun param => .ok (call.passing.valuesAt call.alignment inputs.values position param))
        pure ⟨shape, values⟩) := by
  simp only [run, CheckedCall.runValues_eq]

theorem CheckedVectorCall.runtime_mismatch (checked : CheckedVectorCall plan)
    (body : ScalarBody parameters t) (sources : VectorCallSources parameters plan)
    (demanded : (shape : Shape) → List (Position shape))
    (evaluated : VectorCallValues parameters plan) (success : sources.evaluate = .ok evaluated)
    (misaligned : evaluated.materialize.align = none) :
    checked.run body sources demanded = .error .vectorLengthMismatch := by
  rw [run, success]
  simp only [bind, Except.bind]
  rw [misaligned]

theorem CheckedVectorCall.run_argument_failure (checked : CheckedVectorCall plan)
    (body : ScalarBody parameters t) (sources : VectorCallSources parameters plan)
    (demanded : (shape : Shape) → List (Position shape)) {failure : Failure}
    (failed : sources.evaluate = .error failure) :
    checked.run body sources demanded = .error failure := by
  rw [run, failed]
  rfl

theorem VectorCallValues.materialized_shapes (evaluated : VectorCallValues parameters plan) :
    ∀ shape ∈ evaluated.materialize.shapes, shape = .scalar ∨ ∃ n, shape = .vector n := by
  induction evaluated with
  | nil => intro _ member; cases member
  | whole _ rest correct =>
    intro shape member
    rcases List.mem_cons.mp member with rfl | inTail
    · exact Or.inl rfl
    · exact correct shape inTail
  | lifted extent values rest correct =>
    intro shape member
    change shape ∈ .vector (extent.materialized values).1 :: rest.materialize.shapes at member
    rcases List.mem_cons.mp member with rfl | inTail
    · exact Or.inr ⟨_, rfl⟩
    · exact correct shape inTail

theorem VectorCallValues.align_rejects_iff (evaluated : VectorCallValues parameters plan) :
    evaluated.materialize.align = none ↔
      ¬ ∃ size, ∀ shape ∈ evaluated.materialize.shapes, shape = .scalar ∨ shape = .vector size := by
  simp only [MaterializedCall.align, Option.map_eq_none_iff, AlignArguments.infer_rejects_iff,
    AlignArguments.vector_alignment_iff evaluated.materialized_shapes]

def VectorPassing.staticallySized : VectorPassing → Prop
  | .lifted .runtime => False
  | .whole | .lifted (.known _) => True

theorem VectorCallValues.materialized_shapes_eq_fixed (evaluated : VectorCallValues parameters plan)
    (sized : ∀ mode ∈ plan, mode.staticallySized) :
    evaluated.materialize.shapes = VectorPassing.fixedShapes plan := by
  induction evaluated with
  | nil => rfl
  | whole value rest correct =>
    exact congrArg (List.cons Shape.scalar) (correct (fun mode member =>
      sized mode (List.mem_cons_of_mem _ member)))
  | lifted extent values rest correct =>
    cases extent with
    | known size =>
      exact congrArg (List.cons (Shape.vector size)) (correct (fun mode member =>
        sized mode (List.mem_cons_of_mem _ member)))
    | runtime => exact False.elim (sized _ List.mem_cons_self)

theorem CheckedVectorCall.statically_sized_align (checked : CheckedVectorCall plan)
    (evaluated : VectorCallValues parameters plan)
    (sized : ∀ mode ∈ plan, mode.staticallySized) :
    evaluated.materialize.align.isSome = true := by
  simp only [MaterializedCall.align, Option.isSome_map]
  rw [evaluated.materialized_shapes_eq_fixed sized, checked.staticAlignment.infer_complete]
  rfl

end Rupam
