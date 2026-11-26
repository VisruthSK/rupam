import Rupam.Deferred

namespace Rupam

-- Filtering keeps a subsequence of the input observations in a fresh domain.
structure RowInclusion (source target : Domain) where
  sourceRow : Row target → Row source
  ordered : ∀ {a b}, a.val < b.val → (sourceRow a).val < (sourceRow b).val
  fresh : target.identity ≠ source.identity

def Stage.restrict (source : Stage) (inclusion : RowInclusion source.domain target) : Stage where
  domain := target
  schema := source.schema
  read name t present row := source.read name t present (inclusion.sourceRow row)

theorem RowInclusion.cannot_align (inclusion : RowInclusion source target)
    (alignment : Align (.column source) (.column target) result) : False := by
  have sameDomain := alignment.columns_same_domain.1
  exact inclusion.fresh (congrArg Domain.identity sameDomain.symm)

theorem DeferredReference.restrict_at (reference : DeferredReference captured t)
    (lexical : ArgumentValues captured) (source : Stage)
    (inclusion : RowInclusion source.domain target)
    {original : LiftArgument t (.column source.domain)} {restricted : LiftArgument t (.column target)}
    (before : reference.bind lexical source = .ok original)
    (after : reference.bind lexical (source.restrict inclusion) = .ok restricted)
    (row : Row target) :
    restricted.valueAt row = original.valueAt (inclusion.sourceRow row) := by
  cases reference with
  | lexical identity =>
    have originalValue := Except.ok.inj (before.symm.trans (lexical_stable identity lexical source))
    have restrictedValue := Except.ok.inj
      (after.symm.trans (lexical_stable identity lexical (source.restrict inclusion)))
    rw [originalValue, restrictedValue]
    rfl
  | column name =>
    have present : source.schema name = some t :=
      ((DeferredReference.column name : DeferredReference captured t).bind_succeeds_iff lexical source).mp
        ⟨original, before⟩
    have originalRead := Except.ok.inj (before.symm.trans (column_at name lexical source present))
    have restrictedRead := Except.ok.inj
      (after.symm.trans (column_at name lexical (source.restrict inclusion) present))
    rw [originalRead, restrictedRead]
    rfl

theorem DeferredColumn.restrict_lifting (closure : DeferredColumn t) (source : Stage)
    (inclusion : RowInclusion source.domain target)
    {original : BoundInputs (.column source.domain) closure.parameters}
    {restricted : BoundInputs (.column target) closure.parameters}
    (before : closure.inputs.bind source closure.lexical = .ok original)
    (after : closure.inputs.bind (source.restrict inclusion) closure.lexical = .ok restricted)
    (row : Row target) :
    (closure.body.lower fun param => (restricted.argument param).toCore).eval row =
      (closure.body.lower fun param => (original.argument param).toCore).eval
        (inclusion.sourceRow row) := by
  rw [closure.body.lifting restricted.argument row,
    closure.body.lifting original.argument (inclusion.sourceRow row)]
  congr 1
  funext _ param
  exact congrArg Except.ok ((closure.inputs.reference param).restrict_at closure.lexical
    source inclusion (closure.inputs.bind_at source closure.lexical before param)
    (closure.inputs.bind_at (source.restrict inclusion) closure.lexical after param) row)

end Rupam
