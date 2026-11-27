namespace Rupam.Specification

-- Each semantics supplies its own execution relation over a common observation
-- type. A terminating failure is an observation too. The relation can admit
-- several outcomes when the spec permits variation across executions.
abbrev Execution (Program Inputs Observation : Type) :=
  Program → Inputs → Observation → Prop

-- A checker produces a Core term and a judgment. The declarative relation
-- records the judgment, including requirements, independently of the checker.
def InferenceSound (infer : Context → Source → Except Diagnostic (Core × Judgment))
    (derives : Context → Source → Core → Judgment → Prop) : Prop :=
  ∀ {context source core judgment}, infer context source = .ok (core, judgment) →
    derives context source core judgment

def RequirementsExact (infers : Core → Requirement → Prop)
    (uses : Core → Requirement → Prop) : Prop :=
  ∀ core requirement, infers core requirement ↔ uses core requirement

def Equivalent (source : Execution Source Inputs Observation)
    (target : Execution Target Inputs Observation) (translate : Source → Target → Prop)
    (admissible : Source → Inputs → Prop) : Prop :=
  ∀ {program compiled inputs}, translate program compiled → admissible program inputs →
    ∀ observed, source program inputs observed ↔ target compiled inputs observed

theorem Equivalent.coherent
    {source : Execution Source Inputs Observation} {core : Execution Core Inputs Observation}
    {elaborates : Source → Core → Prop} {admissible : Source → Inputs → Prop}
    (equivalent : Equivalent source core elaborates admissible)
    (first : elaborates program left) (second : elaborates program right)
    (ready : admissible program inputs) (observed : Observation) :
    core left inputs observed ↔ core right inputs observed :=
  (equivalent first ready observed).symm.trans (equivalent second ready observed)

-- For native execution every outcome must be allowed by the source semantics,
-- and the translation must admit an execution on each admissible input.
def Preserves (source : Execution Source Inputs Observation)
    (target : Execution Target Inputs Observation) (translate : Source → Target → Prop)
    (admissible : Source → Inputs → Prop) : Prop :=
  ∀ {program compiled inputs}, translate program compiled → admissible program inputs →
    (∃ observed, target compiled inputs observed) ∧
    (∀ observed, target compiled inputs observed → source program inputs observed)

def SourceSafe (source : Execution Source Inputs Observation)
    (admissible : Source → Inputs → Prop) (permitted : Source → Inputs → Observation → Prop) : Prop :=
  ∀ {program inputs}, admissible program inputs →
    ∀ observed, source program inputs observed → permitted program inputs observed

theorem Preserves.throughElaboration
    {source : Execution Source Inputs Observation} {core : Execution Core Inputs Observation}
    {target : Execution Target Inputs Observation}
    {elaborates : Source → Core → Prop} {lowers : Core → Target → Prop}
    {admissible : Source → Inputs → Prop} {coreAdmissible : Core → Inputs → Prop}
    (elaboration : Equivalent source core elaborates admissible)
    (lowering : Preserves core target lowers coreAdmissible)
    (validCore : ∀ {program checked inputs}, elaborates program checked → admissible program inputs →
      coreAdmissible checked inputs) :
    Preserves source target (fun program compiled => ∃ checked,
      elaborates program checked ∧ lowers checked compiled) admissible := by
  intro program compiled inputs translation ready
  obtain ⟨checked, elaborated, lowered⟩ := translation
  obtain ⟨executes, permitted⟩ := lowering lowered (validCore elaborated ready)
  exact ⟨executes, fun observation ran =>
    (elaboration elaborated ready observation).mpr (permitted observation ran)⟩

theorem Preserves.transfersSafety
    {source : Execution Source Inputs Observation} {target : Execution Target Inputs Observation}
    {translate : Source → Target → Prop} {admissible : Source → Inputs → Prop}
    {permitted : Source → Inputs → Observation → Prop}
    (preserves : Preserves source target translate admissible)
    (safety : SourceSafe source admissible permitted)
    (translated : translate program compiled) (ready : admissible program inputs) :
    (∃ observed, target compiled inputs observed) ∧
    (∀ observed, target compiled inputs observed → permitted program inputs observed) := by
  obtain ⟨executes, allowed⟩ := preserves translated ready
  exact ⟨executes, fun observed ran => safety ready observed (allowed observed ran)⟩

end Rupam.Specification
