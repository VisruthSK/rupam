import Rupam.Deferred
import Init.Data.Vector.OfFn

namespace Rupam

def Stage.withColumn (source : Stage) (name : String) (t : Ty)
    (cells : Row source.domain → t.denote) : Stage where
  domain := source.domain
  schema key := if key = name then some t else source.schema key
  read := fun key expected found row => by
    change (if key = name then some t else source.schema key) = some expected at found
    by_cases same : key = name
    · rw [ite_eq_left same] at found
      have matching : t = expected := Option.some.inj found
      exact matching ▸ cells row
    · rw [ite_eq_right same] at found
      exact source.read key expected found row

theorem Stage.withColumn_reads_new (source : Stage) (name : String) (t : Ty)
    (cells : Row source.domain → t.denote) (row : Row source.domain)
    (present : (source.withColumn name t cells).schema name = some t) :
    (source.withColumn name t cells).read name t present row = cells row := by
  simp [withColumn]

theorem Stage.withColumn_reads_old (source : Stage) (name other : String) (t oldType : Ty)
    (cells : Row source.domain → t.denote) (row : Row source.domain)
    (unrelated : other ≠ name) (before : source.schema other = some oldType)
    (after : (source.withColumn name t cells).schema other = some oldType) :
    (source.withColumn name t cells).read other oldType after row =
      source.read other oldType before row := by
  simp [withColumn, unrelated]

structure Mutation (source : Stage) (name : String) (t : Ty) where
  values : Vector t.denote source.domain.size

def Mutation.stage (result : Mutation source name t) : Stage :=
  source.withColumn name t result.values.get

def Stage.mutate (source : Stage) (name : String)
    (expression : Expr (.column source.domain) t) : Outcome (Mutation source name t) := do
  let values ← Vector.ofFnM expression.eval
  pure ⟨values⟩

theorem Stage.mutate_constant (source : Stage) (name : String) {t : Ty} (value : t.denote) :
    source.mutate name (.broadcast (.literal value)) =
      .ok ⟨Vector.ofFn (fun _ => value)⟩ := by
  unfold mutate
  change (do
    let values ← Vector.ofFnM (m := Outcome) (fun _ => pure value)
    pure (Mutation.mk values)) = _
  rw [Vector.ofFnM_pure]
  rfl

theorem Mutation.keeps_entry_group (result : Mutation source name t) (group : Group source.domain) :
    ∃ after : Group result.stage.domain,
      after.rows = group.rows :=
  ⟨group, rfl⟩

theorem Stage.mutate_failing (source : Stage) (name : String)
    (expression : Expr (.column source.domain) t) {failure : Failure}
    (failed : Vector.ofFnM expression.eval = .error failure) :
    source.mutate name expression = .error failure := by
  unfold mutate
  rw [failed]
  rfl

end Rupam
