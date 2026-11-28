import Rupam.QualificationCheck

namespace Rupam

inductive SchemeSort where
  | base
  | cell
  | shape
  | requirements
  | flag
  | type

abbrev SchemeSort.denote : SchemeSort → Type
  | .base => Base
  | .cell => Ty
  | .shape => Shape
  | .requirements => List Requirement
  | .flag => Bool
  | .type => QualifiedType

inductive SchemeVariable : List SchemeSort → SchemeSort → Type where
  | first : SchemeVariable (sort :: rest) sort
  | next : SchemeVariable rest sort → SchemeVariable (other :: rest) sort

def SchemeVariable.weaken (slot : SchemeVariable scope sort) :
    (added : List SchemeSort) → SchemeVariable (added ++ scope) sort
  | [] => slot
  | _ :: rest => .next (slot.weaken rest)

abbrev SchemeValuation (scope : List SchemeSort) :=
  ∀ {sort}, SchemeVariable scope sort → sort.denote

def SchemeValuation.lexical (valuation : SchemeValuation (quantified ++ scope)) :
    SchemeValuation scope :=
  fun slot => valuation (slot.weaken quantified)

inductive SchemeTerm (scope : List SchemeSort) : SchemeSort → Type where
  | variable : SchemeVariable scope sort → SchemeTerm scope sort
  | constant : sort.denote → SchemeTerm scope sort
  | required : SchemeTerm scope .base → SchemeTerm scope .cell
  | optional : SchemeTerm scope .base → SchemeTerm scope .cell
  | data : SchemeTerm scope .shape → SchemeTerm scope .cell → SchemeTerm scope .type
  | calculation : SchemeTerm scope .cell → SchemeTerm scope .type
  | pointwise : SchemeTerm scope .cell → SchemeTerm scope .cell → SchemeTerm scope .type
  | contextual : SchemeTerm scope .requirements → SchemeTerm scope .type → SchemeTerm scope .type →
      SchemeTerm scope .type

def SchemeTerm.eval (valuation : SchemeValuation scope) : SchemeTerm scope sort → sort.denote
  | .variable slot => valuation slot
  | .constant value => value
  | .required base => .required (base.eval valuation)
  | .optional base => .optional (base.eval valuation)
  | .data shape cell => .data (shape.eval valuation) (cell.eval valuation)
  | .calculation cell => .calculation (cell.eval valuation)
  | .pointwise input output => .pointwise (input.eval valuation) (output.eval valuation)
  | .contextual needs input output =>
    .contextual (needs.eval valuation) (input.eval valuation) (output.eval valuation)

abbrev SchemeSubstitution (source target : List SchemeSort) :=
  ∀ {sort}, SchemeVariable source sort → SchemeTerm target sort

private def SchemeSubstitution.cons (head : SchemeTerm target sort)
    (tail : SchemeSubstitution source target) : SchemeSubstitution (sort :: source) target
  | _, .first => head
  | _, .next slot => tail slot

def SchemeSubstitution.valuation (replace : SchemeSubstitution source target)
    (valuation : SchemeValuation target) : SchemeValuation source :=
  fun slot => (replace slot).eval valuation

def SchemeTerm.substitute (replace : SchemeSubstitution source target) :
    SchemeTerm source sort → SchemeTerm target sort
  | .variable slot => replace slot
  | .constant value => .constant value
  | .required base => .required (base.substitute replace)
  | .optional base => .optional (base.substitute replace)
  | .data shape cell => .data (shape.substitute replace) (cell.substitute replace)
  | .calculation cell => .calculation (cell.substitute replace)
  | .pointwise input output => .pointwise (input.substitute replace) (output.substitute replace)
  | .contextual needs input output =>
    .contextual (needs.substitute replace) (input.substitute replace) (output.substitute replace)

theorem SchemeTerm.eval_substitute (term : SchemeTerm source sort)
    (replace : SchemeSubstitution source target) (valuation : SchemeValuation target) :
    (term.substitute replace).eval valuation = term.eval (replace.valuation valuation) := by
  induction term <;> simp_all [substitute, eval, SchemeSubstitution.valuation]

structure SchemeInterface (scope : List SchemeSort) where
  type : SchemeTerm scope .type
  needs : SchemeTerm scope .requirements
  scalarOnly : SchemeTerm scope .flag

def SchemeInterface.eval (interface : SchemeInterface scope) (valuation : SchemeValuation scope) :
    QualifiedJudgment :=
  ⟨interface.type.eval valuation, interface.needs.eval valuation, interface.scalarOnly.eval valuation⟩

def SchemeInterface.substitute (interface : SchemeInterface source)
    (replace : SchemeSubstitution source target) : SchemeInterface target :=
  ⟨interface.type.substitute replace, interface.needs.substitute replace,
    interface.scalarOnly.substitute replace⟩

-- Residual schemes retain qualifiers. Solving or simplifying them can
-- restrict how they are displayed, but must preserve their instances.
structure ResidualScheme (scope : List SchemeSort) where
  quantified : List SchemeSort
  interface : SchemeInterface (quantified ++ scope)
  qualifies : SchemeValuation (quantified ++ scope) → Qualification

def ResidualScheme.Sound (scheme : ResidualScheme scope) (source : QualifiedSource)
    (environment : SchemeValuation scope → List QualifiedBinding) : Prop :=
  ∀ valuation, (scheme.qualifies valuation).Holds →
    Qualifies (environment valuation.lexical) source (scheme.interface.eval valuation)

structure ResidualScheme.Instance (general specific : ResidualScheme scope) where
  substitute : SchemeSubstitution (general.quantified ++ scope) (specific.quantified ++ scope)
  fixesLexical : ∀ {sort} (slot : SchemeVariable scope sort),
    substitute (slot.weaken general.quantified) = .variable (slot.weaken specific.quantified)
  interface_eq : general.interface.substitute substitute = specific.interface
  entails : ∀ valuation, (specific.qualifies valuation).Holds →
    (general.qualifies (substitute.valuation valuation)).Holds

private def freeInterface : SchemeInterface (.type :: .requirements :: .flag :: scope) :=
  ⟨.variable .first, .variable (.next .first), .variable (.next (.next .first))⟩

def QualifiedSource.residual (source : QualifiedSource)
    (environment : SchemeValuation scope → List QualifiedBinding) : ResidualScheme scope :=
  { quantified := [.type, .requirements, .flag]
    interface := freeInterface
    qualifies := fun valuation =>
      (source.generate (environment valuation.lexical) (freeInterface.eval valuation)).reduceFacts }

private def ResidualScheme.factor (specific : ResidualScheme scope) :
    SchemeSubstitution (.type :: .requirements :: .flag :: scope) (specific.quantified ++ scope) :=
  SchemeSubstitution.cons specific.interface.type
    (SchemeSubstitution.cons specific.interface.needs
      (SchemeSubstitution.cons specific.interface.scalarOnly
        (fun slot => .variable (slot.weaken specific.quantified))))

theorem QualifiedSource.residual_sound (source : QualifiedSource)
    (environment : SchemeValuation scope → List QualifiedBinding) :
    (source.residual environment).Sound source environment :=
  fun valuation valid =>
    source.generation_sound (environment valuation.lexical) (freeInterface.eval valuation)
      (Qualification.reduceFacts_preserves _ |>.mp valid)

theorem QualifiedSource.residual_principal (source : QualifiedSource)
    (environment : SchemeValuation scope → List QualifiedBinding)
    (specific : ResidualScheme scope) (sound : specific.Sound source environment) :
    Nonempty ((source.residual environment).Instance specific) := by
  refine ⟨⟨specific.factor, fun _ => rfl, rfl, ?_⟩⟩
  intro valuation valid
  exact (Qualification.reduceFacts_preserves _).mpr (sound valuation valid).satisfies

end Rupam
