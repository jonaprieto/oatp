/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP

open Lean Elab Command Meta

def demoP : Prop := True
def demoQ : Prop := True
def demoPredicate (_ : Nat) : Prop := True

private def runProofDemo : TermElabM Unit := do
  let goal ← mkFreshExprMVar (some (mkConst ``True))
  match ← OATP.Proof.reconstruct goal.mvarId! .trueIntro with
  | .ok _ => logInfo "OATP proof demo: kernel-checked True introduction"
  | .error message => throwError message
  let trueType := mkConst ``True
  let conjunction ← mkAppM ``And #[trueType, trueType]
  let conjunctionGoal ← mkFreshExprMVar (some conjunction)
  match ← OATP.Proof.reconstruct conjunctionGoal.mvarId! (.andIntro .trueIntro .trueIntro) with
  | .ok _ => pure ()
  | .error message => throwError message
  let implication ← mkArrow trueType trueType
  let implicationGoal ← mkFreshExprMVar (some implication)
  match ← OATP.Proof.reconstruct implicationGoal.mvarId!
      (.implicationIntro `h .trueIntro) with
  | .ok _ => pure ()
  | .error message => throwError message
  let p := mkConst ``demoP
  let q := mkConst ``demoQ
  let propositionalTarget ← mkAppM ``And #[p, q]
  let translationGoal ← mkFreshExprMVar (some propositionalTarget)
  match ← OATP.Lean.translateGoal translationGoal.mvarId! with
  | .ok translation =>
      unless translation.target == "(oatp_64656d6f50 & oatp_64656d6f51)" do
        throwError "OATP translation rendered the supported propositional goal incorrectly"
      unless translation.problem.source.contains "fof(goal, conjecture" do
        throwError "OATP translation omitted the conjecture statement"
  | .error message => throwError message
  let unsupportedTarget ← mkAppM ``demoPredicate #[mkNatLit 0]
  let unsupportedGoal ← mkFreshExprMVar (some unsupportedTarget)
  match ← OATP.Lean.translateGoal unsupportedGoal.mvarId! with
  | .error _ => pure ()
  | .ok _ => throwError "OATP translation accepted an unsupported predicate application"
  withLocalDeclD `h trueType fun _ => do
    let localGoal ← mkFreshExprMVar (some trueType)
    let snapshot ← OATP.Lean.snapshot localGoal.mvarId!
    unless snapshot.context.any (·.startsWith "h :") do
      throwError "OATP proof demo lost the local metavariable context"
    match ← OATP.Proof.reconstruct localGoal.mvarId! (.exact `h) with
    | .ok _ => pure ()
    | .error message => throwError message
  let invalidGoal ← mkFreshExprMVar (some trueType)
  match ← OATP.Proof.reconstruct invalidGoal.mvarId! (.andIntro .trueIntro .trueIntro) with
  | .error _ => pure ()
  | .ok _ => throwError "OATP proof demo accepted an invalid reconstruction"

syntax (name := oatpProofDemo) "#oatp_proof_demo" : command

@[command_elab oatpProofDemo]
meta def elabOATPProofDemo : CommandElab := fun stx =>
  match stx with
  | `(command| #oatp_proof_demo) => liftTermElabM runProofDemo
  | _ => throwUnsupportedSyntax

#oatp_proof_demo

def main : IO UInt32 := do
  IO.println "OATP proof demo passed"
  return 0
