import Rupam.HigherOrder

namespace Rupam

abbrev Renaming (source target : List (ContextType atom)) :=
  ∀ {t}, Local source t → Local target t

def Renaming.under (rename : Renaming source target) :
    Renaming (bound :: source) (bound :: target)
  | _, .first => .first
  | _, .next binding => .next (rename binding)

def ContextTerm.rename (rename : Renaming source target) :
    ContextTerm payload source t needs → ContextTerm payload target t needs
  | .variable binding => .variable (rename binding)
  | .scalar value => .scalar value
  | .vector values => .vector values
  | .pointwise operation => .pointwise operation
  | .reducer kind policy operation => .reducer kind policy operation
  | .vectorReducer policy operation => .vectorReducer policy operation
  | .orderedReducer kind policy operation => .orderedReducer kind policy operation
  | .mapResult operation input => .mapResult operation (input.rename rename)
  | .combine operation left right => .combine operation (left.rename rename) (right.rename rename)
  | .conditional test condition yes no =>
    .conditional test (condition.rename rename) (yes.rename rename) (no.rename rename)
  | .lambda body => .lambda (body.rename rename.under)
  | .apply fn argument => .apply (fn.rename rename) (argument.rename rename)

theorem ContextTerm.eval_rename (term : ContextTerm payload source t needs)
    (rename : Renaming source target) (context : ConsumptionContext)
    (environment : ContextEnvironment payload context target) (supplies : Supplies context needs) :
    (term.rename rename).eval context environment supplies =
      term.eval context (fun binding => environment (rename binding)) supplies := by
  induction term generalizing target with
  | «variable» binding => rfl
  | scalar value => rfl
  | vector values => rfl
  | pointwise operation => rfl
  | reducer kind policy operation => rfl
  | vectorReducer policy operation => rfl
  | orderedReducer kind policy operation => rfl
  | mapResult operation input inputAgrees =>
    simp only [ContextTerm.rename, ContextTerm.eval, inputAgrees]
    rfl
  | combine operation left right leftAgrees rightAgrees =>
    simp only [ContextTerm.rename, ContextTerm.eval, leftAgrees, rightAgrees]
    rfl
  | conditional test condition yes no conditionAgrees yesAgrees noAgrees =>
    simp only [ContextTerm.rename, ContextTerm.eval, conditionAgrees, yesAgrees, noAgrees]
    rfl
  | lambda body bodyAgrees =>
    funext forCall argument
    simp only [ContextTerm.rename, ContextTerm.eval, bodyAgrees]
    congr 1
    funext _ binding
    cases binding <;> rfl
  | apply fn argument fnAgrees argAgrees =>
    simp only [ContextTerm.rename, ContextTerm.eval, fnAgrees, argAgrees]

abbrev Substitution (payload : atom → Type) (source target : List (ContextType atom)) :=
  ∀ {t}, Local source t → ContextTerm payload target t []

def Substitution.under (substitute : Substitution payload source target) :
    Substitution payload (bound :: source) (bound :: target)
  | _, .first => .variable .first
  | _, .next binding => (substitute binding).rename (fun reference => .next reference)

def ContextTerm.substitute (replace : Substitution payload source target) :
    ContextTerm payload source t needs → ContextTerm payload target t needs
  | .variable binding => replace binding
  | .scalar value => .scalar value
  | .vector values => .vector values
  | .pointwise operation => .pointwise operation
  | .reducer kind policy operation => .reducer kind policy operation
  | .vectorReducer policy operation => .vectorReducer policy operation
  | .orderedReducer kind policy operation => .orderedReducer kind policy operation
  | .mapResult operation input => .mapResult operation (input.substitute replace)
  | .combine operation left right => .combine operation (left.substitute replace) (right.substitute replace)
  | .conditional test condition yes no =>
    .conditional test (condition.substitute replace) (yes.substitute replace) (no.substitute replace)
  | .lambda body => .lambda (body.substitute replace.under)
  | .apply fn argument => .apply (fn.substitute replace) (argument.substitute replace)

theorem ContextTerm.eval_substitute (term : ContextTerm payload source t needs)
    (replace : Substitution payload source target) (context : ConsumptionContext)
    (environment : ContextEnvironment payload context target) (supplies : Supplies context needs) :
    (term.substitute replace).eval context environment supplies =
      term.eval context (fun binding => (replace binding).eval context environment .nil) supplies := by
  induction term generalizing target with
  | «variable» binding =>
    cases supplies
    rfl
  | scalar value => rfl
  | vector values => rfl
  | pointwise operation => rfl
  | reducer kind policy operation => rfl
  | vectorReducer policy operation => rfl
  | orderedReducer kind policy operation => rfl
  | mapResult operation input inputAgrees =>
    simp only [ContextTerm.substitute, ContextTerm.eval, inputAgrees]
    rfl
  | combine operation left right leftAgrees rightAgrees =>
    simp only [ContextTerm.substitute, ContextTerm.eval, leftAgrees, rightAgrees]
    rfl
  | conditional test condition yes no conditionAgrees yesAgrees noAgrees =>
    simp only [ContextTerm.substitute, ContextTerm.eval, conditionAgrees, yesAgrees, noAgrees]
    rfl
  | lambda body bodyAgrees =>
    funext forCall argument
    simp only [ContextTerm.substitute, ContextTerm.eval, bodyAgrees]
    congr 1
    funext _ binding
    cases binding with
    | first => rfl
    | next reference =>
      rw [Substitution.under, ContextTerm.eval_rename]
      rfl
  | apply fn argument fnAgrees argAgrees =>
    simp only [ContextTerm.substitute, ContextTerm.eval, fnAgrees, argAgrees]

def Substitution.inline (argument : ContextTerm payload locals input []) :
    Substitution payload (input :: locals) locals
  | _, .first => argument
  | _, .next binding => .variable binding

theorem ContextTerm.inlining (body : ContextTerm payload (input :: locals) output latent)
    (argument : ContextTerm payload locals input [])
    (context : ConsumptionContext) (environment : ContextEnvironment payload context locals)
    (supplies : Supplies context latent) :
    (ContextTerm.apply (.lambda body) argument).eval context environment supplies =
      (body.substitute (Substitution.inline argument)).eval context environment supplies := by
  rw [ContextTerm.apply_lambda, ContextTerm.eval_substitute]
  change body.eval context (environment.push (argument.eval context environment .nil)) supplies = _
  congr 1
  funext _ binding
  cases binding <;> rfl

end Rupam
