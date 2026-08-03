/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Lean
import OATP.Lean

/-!
# OATP.Proof: small kernel-checked reconstruction calculus

This is the first proof-term boundary, not an ATP proof format. It covers
structural propositional steps that an adapter can translate into safely:
local hypotheses, `True`, conjunctions, and implications.
-/

namespace OATP.Proof

open _root_.Lean
open _root_.Lean.Meta

inductive Step where
  | exact (localName : Name)
  | trueIntro
  | andIntro (left right : Step)
  | andLeft (localName : Name)
  | andRight (localName : Name)
  | implicationIntro (localName : Name) (body : Step)
  deriving Repr

private def andParts (target : Expr) : MetaM (Option (Expr × Expr)) := do
  match ← whnf target with
  | .app (.app (.const ``And _) left) right => pure (some (left, right))
  | _ => pure none

private def projection (target : Expr) (localName : Name) (useLeft : Bool) :
    MetaM (Except String Expr) := do
  let hypothesis ← getFVarFromUserName localName
  match ← andParts (← inferType hypothesis) with
  | none => pure (.error s!"local hypothesis `{localName}` is not a conjunction")
  | some (left, right) =>
      let selected := if useLeft then left else right
      if ← isDefEq selected target then
        let theoremName := if useLeft then ``And.left else ``And.right
        pure (.ok (← mkAppM theoremName #[hypothesis]))
      else
        pure (.error s!"conjunction projection from `{localName}` has the wrong target")

private def build (target : Expr) : Step → MetaM (Except String Expr)
  | .exact localName => do
      let hypothesis ← getFVarFromUserName localName
      let hypothesisType ← inferType hypothesis
      if ← isDefEq hypothesisType target then
        pure (.ok hypothesis)
      else
        pure (.error s!"local hypothesis `{localName}` does not prove the target")
  | .trueIntro => do
      if ← isDefEq target (mkConst ``True) then
        pure (.ok (mkConst ``True.intro))
      else
        pure (.error "true-intro requires target True")
  | .andIntro left right => do
      match ← andParts target with
      | none => pure (.error "and-intro requires a conjunction target")
      | some (leftTarget, rightTarget) =>
          match ← build leftTarget left, ← build rightTarget right with
          | .ok leftProof, .ok rightProof =>
              pure (.ok (← mkAppM ``And.intro #[leftProof, rightProof]))
          | .error message, _ | _, .error message => pure (.error message)
  | .andLeft localName => projection target localName true
  | .andRight localName => projection target localName false
  | .implicationIntro localName body => do
      match ← whnf target with
      | .forallE _ premise conclusion _ =>
          withLocalDeclD localName premise fun localVar => do
            match ← build (conclusion.instantiate1 localVar) body with
            | .error message => pure (.error message)
            | .ok proof => pure (.ok (← mkLambdaFVars #[localVar] proof))
      | _ => pure (.error "implication-intro requires an implication target")

def reconstruct (mvarId : MVarId) (step : Step) :
    MetaM (Except String OATP.Lean.CheckedProof) := mvarId.withContext do
  let target ← instantiateMVars (← mvarId.getType)
  match ← build target step with
  | .error message => pure (.error message)
  | .ok proof => OATP.Lean.checkAndAssign mvarId proof

end OATP.Proof
