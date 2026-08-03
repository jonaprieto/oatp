/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Lean
import OATP.Events

/-!
# OATP.Lean: the kernel boundary

This module is intentionally small. It extracts renderer-independent goal
snapshots from Lean metavariables and checks candidate proof terms before
assigning them. External ATP output never enters this API as a proof term.
-/

namespace OATP.Lean

open _root_.Lean
open _root_.Lean.Meta

structure CheckedProof where
  proof : Expr
  target : Expr

private def prettyExpr (expression : Expr) : MetaM String := do
  let formatted ← ppExpr expression
  pure s!"{formatted}"

def snapshot (mvarId : MVarId) : MetaM GoalSnapshot := mvarId.withContext do
  let target ← instantiateMVars (← mvarId.getType)
  let lctx ← getLCtx
  let context ← lctx.getFVarIds.toList.mapM fun fvarId => do
    let declaration := lctx.get! fvarId
    let type ← prettyExpr declaration.type
    pure s!"{declaration.userName} : {type}"
  pure {
    title := "Lean goal"
    context := context.toArray
    target := ← prettyExpr target
  }

def check (mvarId : MVarId) (candidate : Expr) : MetaM (Except String CheckedProof) :=
  mvarId.withContext do
  let target ← instantiateMVars (← mvarId.getType)
  let candidate ← instantiateMVars candidate
  let candidateType ← inferType candidate
  if ← isDefEq candidateType target then
    pure (.ok { proof := candidate, target })
  else
    let expected ← prettyExpr target
    let actual ← prettyExpr candidateType
    pure (.error s!"candidate type mismatch: expected {expected}, got {actual}")

def checkAndAssign (mvarId : MVarId) (candidate : Expr) :
    MetaM (Except String CheckedProof) := mvarId.withContext do
  match ← check mvarId candidate with
  | .error message => pure (.error message)
  | .ok checked =>
      mvarId.assign checked.proof
      pure (.ok checked)

end OATP.Lean
