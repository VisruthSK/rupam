import Rupam.Application

namespace Rupam

-- A stage supplies typed reads only for fields in its own schema and domain.
structure Stage where
  domain : Domain
  schema : String → Option Ty
  read : (name : String) → (t : Ty) → schema name = some t → Row domain → t.denote

inductive ColumnError where
  | absent (name : String)
  | incompatible (name : String) (expected actual : Ty)

inductive DeferredReference (captured : List Ty) : Ty → Type where
  | lexical : Parameter captured t → DeferredReference captured t
  | column : (name : String) → DeferredReference captured t

def DeferredReference.bind (reference : DeferredReference captured t)
    (lexical : ArgumentValues captured) (stage : Stage) :
    Except ColumnError (LiftArgument t (.column stage.domain)) :=
  match reference with
  | .lexical identity => .ok (.scalar (lexical identity))
  | .column name =>
    if present : stage.schema name = some t then
      .ok (.column (stage.read name t present))
    else
      match stage.schema name with
      | none => .error (.absent name)
      | some actual => .error (.incompatible name t actual)

theorem DeferredReference.lexical_stable (identity : Parameter captured t)
    (lexical : ArgumentValues captured) (stage : Stage) :
    (DeferredReference.lexical identity).bind lexical stage = .ok (.scalar (lexical identity)) :=
  rfl

theorem DeferredReference.column_at (name : String) (lexical : ArgumentValues captured)
    (stage : Stage) (present : stage.schema name = some t) :
    (DeferredReference.column name : DeferredReference captured t).bind lexical stage =
      .ok (.column (stage.read name t present)) := by
  simp [DeferredReference.bind, present]

theorem DeferredReference.column_absent (name : String) (lexical : ArgumentValues captured)
    (stage : Stage) (absent : stage.schema name = none) :
    (DeferredReference.column name : DeferredReference captured t).bind lexical stage =
      .error (.absent name) := by
  simp [DeferredReference.bind, absent]

theorem DeferredReference.column_incompatible (name : String) (lexical : ArgumentValues captured)
    (stage : Stage) (actual : stage.schema name = some other) (different : other ≠ t) :
    (DeferredReference.column name : DeferredReference captured t).bind lexical stage =
      .error (.incompatible name t other) := by
  simp [DeferredReference.bind, actual, different]

def DeferredReference.Available (stage : Stage) : DeferredReference captured t → Prop
  | .lexical _ => True
  | .column name => stage.schema name = some t

theorem DeferredReference.bind_succeeds_iff (reference : DeferredReference captured t)
    (lexical : ArgumentValues captured) (stage : Stage) :
    (∃ bound, reference.bind lexical stage = .ok bound) ↔ reference.Available stage := by
  cases reference with
  | lexical identity =>
    exact ⟨fun _ => trivial, fun _ => ⟨.scalar (lexical identity), rfl⟩⟩
  | column name =>
    constructor
    · intro ⟨bound, checked⟩
      by_cases present : stage.schema name = some t
      · exact present
      · simp only [bind, dite_eq_right present] at checked
        cases seen : stage.schema name <;> rw [seen] at checked <;> cases checked
    · intro member
      exact ⟨_, column_at name lexical stage member⟩

inductive DeferredInputs (captured : List Ty) : List Ty → Type where
  | nil : DeferredInputs captured []
  | cons : DeferredReference captured t → DeferredInputs captured rest →
      DeferredInputs captured (t :: rest)

def DeferredInputs.reference : DeferredInputs captured parameters →
    Parameter parameters t → DeferredReference captured t
  | .cons head _, .first => head
  | .cons _ tail, .next param => tail.reference param

inductive BoundInputs (shape : Shape) : List Ty → Type where
  | nil : BoundInputs shape []
  | cons : LiftArgument t shape → BoundInputs shape rest → BoundInputs shape (t :: rest)

def BoundInputs.argument : BoundInputs shape parameters → Parameter parameters t → LiftArgument t shape
  | .cons head _, .first => head
  | .cons _ tail, .next param => tail.argument param

def DeferredInputs.bind (stage : Stage) (lexical : ArgumentValues captured) :
    DeferredInputs captured parameters → Except ColumnError (BoundInputs (.column stage.domain) parameters)
  | .nil => .ok .nil
  | .cons head tail => do
    let first ← head.bind lexical stage
    let rest ← tail.bind stage lexical
    pure (.cons first rest)

theorem DeferredInputs.bind_at (inputs : DeferredInputs captured parameters)
    (stage : Stage) (lexical : ArgumentValues captured)
    {bound : BoundInputs (.column stage.domain) parameters}
    (resolved : inputs.bind stage lexical = .ok bound)
    (param : Parameter parameters t) :
    (inputs.reference param).bind lexical stage = .ok (bound.argument param) := by
  induction inputs with
  | nil => nomatch param
  | cons head tail restCorrect =>
    rw [bind] at resolved
    cases first : head.bind lexical stage with
    | error failure =>
      rw [first] at resolved
      cases resolved
    | ok headValue =>
      rw [first] at resolved
      cases rest : tail.bind stage lexical with
      | error failure =>
        rw [rest] at resolved
        cases resolved
      | ok tailValues =>
        rw [rest] at resolved
        cases resolved
        cases param with
        | first => exact first
        | next param => exact restCorrect rest param

theorem DeferredInputs.bind_succeeds_iff (inputs : DeferredInputs captured parameters)
    (stage : Stage) (lexical : ArgumentValues captured) :
    (∃ bound, inputs.bind stage lexical = .ok bound) ↔
      ∀ {t} (param : Parameter parameters t), (inputs.reference param).Available stage := by
  constructor
  · intro ⟨bound, checked⟩ t param
    exact ((inputs.reference param).bind_succeeds_iff lexical stage).mp
      ⟨bound.argument param, inputs.bind_at stage lexical checked param⟩
  · induction inputs with
    | nil =>
      intro _
      exact ⟨.nil, rfl⟩
    | cons head tail bindTail =>
      intro allAvailable
      obtain ⟨first, readFirst⟩ :=
        (head.bind_succeeds_iff lexical stage).mpr (allAvailable .first)
      obtain ⟨rest, readRest⟩ := bindTail (fun param => allAvailable (.next param))
      refine ⟨.cons first rest, ?_⟩
      rw [bind, readFirst]
      change (do let bound ← tail.bind stage lexical; pure (BoundInputs.cons first bound)) = _
      rw [readRest]
      rfl

-- Every use instantiates the same closure against the consuming schema.
structure DeferredColumn (t : Ty) where
  captured : List Ty
  lexical : ArgumentValues captured
  parameters : List Ty
  inputs : DeferredInputs captured parameters
  body : ScalarBody parameters t

def DeferredColumn.lower (closure : DeferredColumn t) (stage : Stage) :
    Except ColumnError (Expr (.column stage.domain) t) := do
  let arguments ← closure.inputs.bind stage closure.lexical
  pure (closure.body.lower fun param => (arguments.argument param).toCore)

theorem DeferredColumn.lifting_at (closure : DeferredColumn t) (stage : Stage)
    {bound : BoundInputs (.column stage.domain) closure.parameters}
    (resolved : closure.inputs.bind stage closure.lexical = .ok bound)
    (row : Row stage.domain) :
    closure.lower stage =
      .ok (closure.body.lower fun param => (bound.argument param).toCore) ∧
    (closure.body.lower fun param => (bound.argument param).toCore).eval row =
      closure.body.eval (fun param => .ok ((bound.argument param).valueAt row)) := by
  constructor
  · unfold DeferredColumn.lower
    rw [resolved]
    rfl
  · exact closure.body.lifting bound.argument row

end Rupam
