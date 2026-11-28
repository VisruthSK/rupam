import Rupam.OrdinaryInference

namespace Rupam

inductive BindingSlot (name : String) : List String → Nat → Type where
  | nearest : BindingSlot name (name :: outer) 0
  | enclosing (different : inner ≠ name) :
      BindingSlot name outer slot → BindingSlot name (inner :: outer) (slot + 1)

def BindingSlot.lookup (name : String) : (names : List String) →
    Option (Σ slot, BindingSlot name names slot)
  | [] => none
  | inner :: outer =>
    if same : inner = name then
      some ⟨0, same.symm ▸ BindingSlot.nearest⟩
    else
      (lookup name outer).map (fun found => ⟨found.1 + 1, .enclosing same found.2⟩)

theorem BindingSlot.lookup_complete (binding : BindingSlot name names slot) :
    BindingSlot.lookup name names = some ⟨slot, binding⟩ := by
  induction binding with
  | nearest => simp [lookup]
  | enclosing different outer outerFound =>
    simp [lookup, different, outerFound]

theorem BindingSlot.in_range (binding : BindingSlot name names slot) :
    slot < names.length := by
  induction binding with
  | nearest => exact Nat.zero_lt_succ _
  | enclosing _ _ outer => exact Nat.succ_lt_succ outer

inductive NamedOrdinary where
  | literal (type : Ty) (value : type.denote)
  | reference (name : String)
  | lambda (name : String) (body : NamedOrdinary)
  | apply (function argument : NamedOrdinary)

inductive ResolvesOrdinary : List String → NamedOrdinary → OrdinarySource → Type where
  | literal (type : Ty) (value : type.denote) :
      ResolvesOrdinary names (.literal type value) (.literal type value)
  | reference : BindingSlot name names slot →
      ResolvesOrdinary names (.reference name) (.reference slot)
  | lambda : ResolvesOrdinary (name :: names) body resolved →
      ResolvesOrdinary names (.lambda name body) (.lambda resolved)
  | apply : ResolvesOrdinary names fn fnResolved → ResolvesOrdinary names argument argResolved →
      ResolvesOrdinary names (.apply fn argument) (.apply fnResolved argResolved)

inductive NamedInferenceError where
  | unbound (name : String)
  | inference (problem : OrdinaryInferenceError)

def NamedOrdinary.resolve (names : List String) :
    (source : NamedOrdinary) → Except NamedInferenceError (Σ resolved, ResolvesOrdinary names source resolved)
  | .literal t value => .ok ⟨.literal t value, .literal t value⟩
  | .reference name =>
    match BindingSlot.lookup name names with
    | none => .error (.unbound name)
    | some ⟨slot, witness⟩ => .ok ⟨.reference slot, .reference witness⟩
  | .lambda name body => do
    let ⟨resolved, proof⟩ ← body.resolve (name :: names)
    pure ⟨.lambda resolved, .lambda proof⟩
  | .apply fn argument => do
    let ⟨left, fnResolved⟩ ← fn.resolve names
    let ⟨right, argResolved⟩ ← argument.resolve names
    pure ⟨.apply left right, .apply fnResolved argResolved⟩

theorem ResolvesOrdinary.resolve_agrees (resolved : ResolvesOrdinary names source term) :
    source.resolve names = .ok ⟨term, resolved⟩ := by
  induction resolved with
  | literal type value => rfl
  | reference binding =>
    rw [NamedOrdinary.resolve, binding.lookup_complete]
  | lambda body bodyAgrees =>
    rw [NamedOrdinary.resolve, bodyAgrees]
    rfl
  | apply fn argument fnAgrees argAgrees =>
    rw [NamedOrdinary.resolve, fnAgrees, argAgrees]
    rfl

structure NamedInferred (bindings : List (String × Monotype)) (source : NamedOrdinary) where
  term : OrdinarySource
  resolves : ResolvesOrdinary (bindings.map Prod.fst) source term
  inferred : OrdinaryInferred (bindings.map Prod.snd) term

def NamedOrdinary.infer (source : NamedOrdinary) (bindings : List (String × Monotype))
    (fuel : Nat) : Except NamedInferenceError (NamedInferred bindings source) := do
  let ⟨term, resolves⟩ ← source.resolve (bindings.map Prod.fst)
  let checked ← (term.infer (bindings.map Prod.snd) fuel).mapError .inference
  pure ⟨term, resolves, checked⟩

end Rupam
