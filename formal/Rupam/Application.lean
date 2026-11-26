import Rupam.Scope

namespace Rupam

variable {scope : EvaluationScope}

inductive ScopedArguments (scope : EvaluationScope) : List Ty → Type where
  | nil : ScopedArguments scope []
  | cons : ScopedCalculation scope t.denote → ScopedArguments scope rest →
      ScopedArguments scope (t :: rest)

abbrev ArgumentValues (parameters : List Ty) :=
  ∀ {t}, Parameter parameters t → t.denote

private def ArgumentValues.cons (head : t.denote) (tail : ArgumentValues rest) :
    ArgumentValues (t :: rest)
  | _, .first => head
  | _, .next param => tail param

-- Ordinary arguments are evaluated in order. The body receives values even
-- if a parameter is used more than once or only under a guard.
def ScopedArguments.run (rows : List scope.Member) :
    ScopedArguments scope parameters → Outcome (ValuesAt rows (ArgumentValues parameters))
  | .nil => .ok (fun _ _ _ param => nomatch param)
  | .cons head tail => do
    let first ← head.run rows
    let rest ← tail.run rows
    pure fun row member => ArgumentValues.cons (first row member) (rest row member)

def ScopedCalculation.call (body : ScalarBody parameters t)
    (arguments : ScopedArguments scope parameters) : ScopedCalculation scope t.denote :=
  ⟨fun first rest => do
    let rows := first :: rest
    let values ← arguments.run rows
    ValuesAt.collect rows fun row member => body.eval (fun param => .ok (values row member param))⟩

theorem ScopedCalculation.call_at (body : ScalarBody parameters t)
    (arguments : ScopedArguments scope parameters) (rows : List scope.Member)
    {values : ValuesAt rows (ArgumentValues parameters)} {output : ValuesAt rows t.denote}
    (argumentsRead : arguments.run rows = .ok values)
    (called : (call body arguments).run rows = .ok output)
    (row : scope.Member) (member : row ∈ rows) :
    body.eval (fun param => .ok (values row member param)) = .ok (output row member) := by
  cases rows with
  | nil => exact False.elim (List.not_mem_nil member)
  | cons first rest =>
    change (do
      let values ← arguments.run (first :: rest)
      ValuesAt.collect (first :: rest) fun r h => body.eval (fun p => .ok (values r h p))) =
        .ok output at called
    rw [argumentsRead] at called
    exact ValuesAt.collect_at called row member

theorem ScopedCalculation.call_argument_failure (body : ScalarBody parameters t)
    (arguments : ScopedArguments scope parameters)
    (first : scope.Member) (rest : List scope.Member) {failure : Failure}
    (failed : arguments.run (first :: rest) = .error failure) :
    (call body arguments).run (first :: rest) = .error failure := by
  -- The helper body cannot catch a failure while evaluating an ordinary argument.
  change (do
    let values ← arguments.run (first :: rest)
    ValuesAt.collect (first :: rest) fun row member =>
      body.eval (fun param => .ok (values row member param))) = .error failure
  rw [failed]
  rfl

-- Source-level ordinary arguments are evaluated before choosing positions
-- at which to run the lifted body. Each yields a materialised value.
inductive OrdinaryArguments (scope : EvaluationScope) : List Ty → Type where
  | nil : OrdinaryArguments scope []
  | cons : (Unit → Outcome (LiftArgument t scope.shape)) → OrdinaryArguments scope rest →
      OrdinaryArguments scope (t :: rest)

def OrdinaryArguments.evaluate :
    OrdinaryArguments scope parameters → Outcome (ScopedArguments scope parameters)
  | .nil => .ok .nil
  | .cons head tail => do
    let value ← head ()
    let rest ← tail.evaluate
    pure (.cons (ScopedCalculation.ofCore value.toCore) rest)

def ScalarBody.callOrdinary (body : ScalarBody parameters t)
    (arguments : OrdinaryArguments scope parameters) (positions : List scope.Member) :
    Outcome (ValuesAt positions t.denote) := do
  let values ← arguments.evaluate
  (ScopedCalculation.call body values).run positions

theorem ScalarBody.callOrdinary_argument_failure (body : ScalarBody parameters t)
    (arguments : OrdinaryArguments scope parameters) (positions : List scope.Member)
    {failure : Failure} (failed : arguments.evaluate = .error failure) :
    body.callOrdinary arguments positions = .error failure := by
  unfold callOrdinary
  rw [failed]
  rfl

end Rupam
