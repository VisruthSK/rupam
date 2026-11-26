import Rupam.Core

namespace Rupam

inductive PolicyDemand where
  | fixed (policy : MissingPolicy)
  | unresolved

-- A policy supplied at one consumption site stays fixed when passed onward.
def PolicyDemand.supply (demand : PolicyDemand) (available : Option MissingPolicy) :
    PolicyDemand :=
  match demand, available with
  | .fixed policy, _ => .fixed policy
  | .unresolved, some policy => .fixed policy
  | .unresolved, none => .unresolved

def PolicyDemand.capture (explicit lexical : Option MissingPolicy) : PolicyDemand :=
  PolicyDemand.unresolved.supply (explicit <|> lexical)

inductive PolicyError where
  | unresolved
  deriving DecidableEq, Repr

def PolicyDemand.discharge : PolicyDemand → Except PolicyError MissingPolicy
  | .fixed policy => .ok policy
  | .unresolved => .error .unresolved

structure PolicyContext where
  call : Option MissingPolicy := none
  verb : Option MissingPolicy := none
  lexical : Option MissingPolicy := none

def PolicyContext.available (context : PolicyContext) : Option MissingPolicy :=
  context.call <|> context.verb <|> context.lexical

def PolicyDemand.resolve (demand : PolicyDemand) (context : PolicyContext) :
    Except PolicyError MissingPolicy :=
  (demand.supply context.available).discharge

-- The four ways the spec permits a decision point's policy to be discharged.
inductive DischargesPolicy (context : PolicyContext) : PolicyDemand → MissingPolicy → Prop where
  | captured (policy : MissingPolicy) : DischargesPolicy context (.fixed policy) policy
  | call (policy : MissingPolicy) (explicit : context.call = some policy) :
      DischargesPolicy context .unresolved policy
  | verb (policy : MissingPolicy) (noCall : context.call = none)
      (explicit : context.verb = some policy) :
      DischargesPolicy context .unresolved policy
  | lexical (policy : MissingPolicy) (noCall : context.call = none)
      (noVerb : context.verb = none) (default : context.lexical = some policy) :
      DischargesPolicy context .unresolved policy

theorem DischargesPolicy.resolve (rule : DischargesPolicy context demand policy) :
    demand.resolve context = .ok policy := by
  cases rule with
  | captured _ =>
    rfl
  | call _ explicit =>
    unfold PolicyDemand.resolve PolicyContext.available
    rw [explicit]
    rfl
  | verb _ noCall explicit =>
    unfold PolicyDemand.resolve PolicyContext.available
    rw [noCall, explicit]
    rfl
  | lexical _ noCall noVerb default =>
    unfold PolicyDemand.resolve PolicyContext.available
    rw [noCall, noVerb, default]
    rfl

theorem PolicyDemand.resolve_sound (resolved : demand.resolve context = .ok policy) :
    DischargesPolicy context demand policy := by
  cases demand with
  | fixed captured =>
    have chosen : captured = policy := Except.ok.inj resolved
    rw [← chosen]
    exact .captured captured
  | unresolved =>
    -- Exhaust the sources in precedence order. A lower source needs evidence
    -- that each higher source is absent.
    unfold PolicyDemand.resolve PolicyContext.available at resolved
    cases call : context.call with
    | some explicit =>
      rw [call] at resolved
      have chosen : explicit = policy := Except.ok.inj resolved
      exact .call policy (call.trans (congrArg some chosen))
    | none =>
      rw [call] at resolved
      cases verb : context.verb with
      | some explicit =>
        rw [verb] at resolved
        have chosen : explicit = policy := Except.ok.inj resolved
        exact .verb policy call (verb.trans (congrArg some chosen))
      | none =>
        rw [verb] at resolved
        cases lexical : context.lexical with
        | some default =>
          rw [lexical] at resolved
          have chosen : default = policy := Except.ok.inj resolved
          exact .lexical policy call verb (lexical.trans (congrArg some chosen))
        | none =>
          rw [lexical] at resolved
          cases resolved

theorem DischargesPolicy.unique (first : DischargesPolicy context demand a)
    (second : DischargesPolicy context demand b) : a = b := by
  -- Both derivations describe the same call to the deterministic resolver.
  have sameResult := first.resolve.symm.trans second.resolve
  exact Except.ok.inj sameResult

theorem PolicyDemand.resolve_failure_iff (demand : PolicyDemand) (context : PolicyContext) :
    demand.resolve context = .error .unresolved ↔
      ¬ ∃ policy, DischargesPolicy context demand policy := by
  constructor
  · intro failure ⟨policy, rule⟩
    have success := rule.resolve
    rw [failure] at success
    cases success
  · intro noRule
    cases resolved : demand.resolve context with
    | ok policy =>
      exact False.elim (noRule ⟨policy, PolicyDemand.resolve_sound resolved⟩)
    | error failure =>
      cases failure
      rfl

theorem PolicyDemand.resolve_fixed (policy : MissingPolicy) (context : PolicyContext) :
    (PolicyDemand.fixed policy).resolve context = .ok policy := rfl

theorem PolicyDemand.resolve_captured_default (policy : MissingPolicy) (caller : PolicyContext) :
    (PolicyDemand.capture none (some policy)).resolve caller = .ok policy := by
  -- The definition's lexical default is captured before considering the caller.
  exact PolicyDemand.resolve_fixed policy caller

theorem PolicyDemand.unresolved_without_context :
    PolicyDemand.unresolved.resolve {} = .error .unresolved := rfl

theorem PolicyDemand.supply_in_order (demand : PolicyDemand)
    (near farther : Option MissingPolicy) :
    (demand.supply near).supply farther = demand.supply (near <|> farther) := by
  cases demand with
  | fixed policy =>
    -- A captured policy ignores both supplies.
    rfl
  | unresolved =>
    cases near with
    | none =>
      -- The requirement is forwarded unchanged to the outer site.
      rfl
    | some policy =>
      -- Supplying the nearer policy makes it fixed.
      rfl

theorem PolicyDemand.resolve_explicit_call (policy : MissingPolicy)
    (verb lexical : Option MissingPolicy) :
    PolicyDemand.unresolved.resolve { call := some policy, verb, lexical } = .ok policy :=
  rfl

structure GroupPolicyContext where
  verbGroup : Option MissingPolicy := none
  verbCommon : Option MissingPolicy := none
  lexicalGroup : Option MissingPolicy := none
  lexicalCommon : Option MissingPolicy := none

def GroupPolicyContext.grouping (context : GroupPolicyContext) : Option MissingPolicy :=
  context.verbGroup <|> context.verbCommon <|> context.lexicalGroup <|> context.lexicalCommon

def GroupPolicyContext.reduction (context : GroupPolicyContext) : Option MissingPolicy :=
  context.verbCommon <|> context.lexicalCommon

theorem GroupPolicyContext.group_override_is_local
    (context : GroupPolicyContext) (policy : MissingPolicy) :
    let updated := { context with verbGroup := some policy }
    updated.grouping = some policy ∧ updated.reduction = context.reduction := by
  constructor
  · rfl
  · rfl

theorem GroupPolicyContext.verb_common_before_lexical_group
    (common : MissingPolicy) (lexicalGroup lexicalCommon : Option MissingPolicy) :
    (GroupPolicyContext.mk none (some common) lexicalGroup lexicalCommon).grouping =
      some common := rfl

end Rupam
