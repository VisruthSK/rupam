import Rupam.QualifiedInference

namespace Rupam

-- Certificates supply the internal interfaces left existential by generation.
-- The checker independently rechecks each type, numeric and call constraint.
inductive QualificationCertificate where
  | fact
  | both (left right : QualificationCertificate)
  | type (instanceType : QualifiedType) (body : QualificationCertificate)
  | judgment (instanceJudgment : QualifiedJudgment) (body : QualificationCertificate)

def Qualification.verify : (constraint : Qualification) → QualificationCertificate →
    Option (PLift constraint.Holds)
  | .truth, .fact => some ⟨True.intro⟩
  | .same left right, .fact =>
    if equal : left = right then some ⟨equal⟩ else none
  | .both left right, .both leftProof rightProof => do
    let l ← left.verify leftProof
    let r ← right.verify rightProof
    pure ⟨l.down, r.down⟩
  | .existsType under, .type instanceType body => do
    let witness ← (under instanceType).verify body
    pure ⟨instanceType, witness.down⟩
  | .existsJudgment under, .judgment instanceJudgment body => do
    let witness ← (under instanceJudgment).verify body
    pure ⟨instanceJudgment, witness.down⟩
  | .application function argument target, .fact => do
    let call ← QualifiedApplies.infer function.type argument.type
    if equal : call.judgment function argument = target then some ⟨call, equal⟩ else none
  | .arithmetic left right target, .fact => do
    let ⟨output, numeric⟩ ← QualifiedNumbers.infer left.type right.type
    if equal : left.arithmetic right output = target then
      some ⟨output, ⟨numeric⟩, equal⟩
    else none
  | _, _ => none

theorem Qualification.verify_complete (constraint : Qualification) (valid : constraint.Holds) :
    ∃ certificate, constraint.verify certificate = some ⟨valid⟩ := by
  induction constraint with
  | truth => exact ⟨.fact, rfl⟩
  | impossible => exact False.elim valid
  | same left right => exact ⟨.fact, by simp [verify, show left = right from valid]⟩
  | both left right leftComplete rightComplete =>
    obtain ⟨l, leftVerified⟩ := leftComplete valid.1
    obtain ⟨r, rightVerified⟩ := rightComplete valid.2
    exact ⟨.both l r, by simp [verify, leftVerified, rightVerified, bind, Option.bind, pure]⟩
  | existsType under complete =>
    obtain ⟨type, instanceValid⟩ := valid
    obtain ⟨certificate, verified⟩ := complete type instanceValid
    exact ⟨.type type certificate, by simp [verify, verified, bind, Option.bind, pure]⟩
  | existsJudgment under complete =>
    obtain ⟨judgment, instanceValid⟩ := valid
    obtain ⟨certificate, verified⟩ := complete judgment instanceValid
    exact ⟨.judgment judgment certificate, by simp [verify, verified, bind, Option.bind, pure]⟩
  | application function argument target =>
    obtain ⟨⟨output, latent, scalar, proof⟩, equal⟩ := valid
    exact ⟨.fact, by simp [verify, proof.infer_complete, bind, Option.bind, equal]⟩
  | arithmetic left right target =>
    obtain ⟨output, ⟨numeric⟩, equal⟩ := valid
    exact ⟨.fact, by simp [verify, numeric.infer_complete, bind, Option.bind, equal]⟩

def QualifiedSource.verify (source : QualifiedSource) (lexical : List QualifiedBinding)
    (judgment : QualifiedJudgment) (certificate : QualificationCertificate) : Bool :=
  ((source.generate lexical judgment).verify certificate).isSome

theorem QualifiedSource.verify_sound (source : QualifiedSource) (lexical : List QualifiedBinding)
    (judgment : QualifiedJudgment) (certificate : QualificationCertificate) :
    source.verify lexical judgment certificate = true → Qualifies lexical source judgment := by
  unfold verify
  cases checked : (source.generate lexical judgment).verify certificate with
  | none => intro impossible; cases impossible
  | some found => exact fun _ => source.generation_sound lexical judgment found.down

theorem QualifiedSource.verify_exists_iff (source : QualifiedSource)
    (lexical : List QualifiedBinding) (judgment : QualifiedJudgment) :
    (∃ certificate, source.verify lexical judgment certificate = true) ↔
      Qualifies lexical source judgment := by
  constructor
  · intro ⟨certificate, checked⟩
    exact source.verify_sound lexical judgment certificate checked
  · intro typed
    obtain ⟨certificate, checked⟩ :=
      (source.generate lexical judgment).verify_complete typed.satisfies
    exact ⟨certificate, by simp [verify, checked]⟩

private def Qualification.atomic : Qualification → Bool
  | .truth | .impossible | .same .. | .application .. | .arithmetic .. => true
  | .both .. | .existsType _ | .existsJudgment _ => false

private theorem Qualification.fact_correct (constraint : Qualification)
    (atomic : constraint.atomic = true) :
    (constraint.verify .fact).isSome = true ↔ constraint.Holds := by
  constructor
  · intro checked
    cases verified : constraint.verify .fact with
    | none => simp [verified] at checked
    | some evidence => exact evidence.down
  · intro holds
    obtain ⟨certificate, verified⟩ := constraint.verify_complete holds
    have isFact : certificate = .fact := by
      cases constraint <;> cases certificate <;>
        simp [Qualification.atomic, Qualification.verify] at atomic verified ⊢
    subst certificate
    simp [verified]

private def Qualification.decideFact (constraint : Qualification) : Qualification :=
  if (constraint.verify .fact).isSome then .truth else .impossible

private theorem Qualification.decideFact_preserves (constraint : Qualification)
    (atomic : constraint.atomic = true) :
    constraint.decideFact.Holds ↔ constraint.Holds := by
  unfold decideFact
  split
  · rename_i accepted
    exact ⟨fun _ => (constraint.fact_correct atomic).mp accepted, fun _ => True.intro⟩
  · rename_i rejected
    exact ⟨False.elim, fun holds => rejected ((constraint.fact_correct atomic).mpr holds)⟩

private def Qualification.conjoin : Qualification → Qualification → Qualification
  | .impossible, _ | _, .impossible => .impossible
  | .truth, right => right
  | left, .truth => left
  | left, right => .both left right

private theorem Qualification.conjoin_preserves (left right : Qualification) :
    (left.conjoin right).Holds ↔ left.Holds ∧ right.Holds := by
  cases left <;> cases right <;> simp [conjoin, Qualification.Holds]

-- Check ground atoms and simplify conjunctions. An existential retains its
-- binder until the solver has evidence for an instantiation.
def Qualification.reduceFacts : Qualification → Qualification
  | .both left right => left.reduceFacts.conjoin right.reduceFacts
  | .existsType under => .existsType fun type => (under type).reduceFacts
  | .existsJudgment under => .existsJudgment fun judgment => (under judgment).reduceFacts
  | fact => fact.decideFact

theorem Qualification.reduceFacts_preserves (constraint : Qualification) :
    constraint.reduceFacts.Holds ↔ constraint.Holds := by
  induction constraint with
  | truth => exact Qualification.decideFact_preserves _ rfl
  | impossible => exact Qualification.decideFact_preserves _ rfl
  | same => exact Qualification.decideFact_preserves _ rfl
  | application => exact Qualification.decideFact_preserves _ rfl
  | arithmetic => exact Qualification.decideFact_preserves _ rfl
  | both left right leftCorrect rightCorrect =>
    exact (Qualification.conjoin_preserves _ _).trans (and_congr leftCorrect rightCorrect)
  | existsType under correct =>
    exact exists_congr correct
  | existsJudgment under correct =>
    exact exists_congr correct

end Rupam
