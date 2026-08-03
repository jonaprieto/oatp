/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP

open Lean Elab Command Meta

private def runProofDemo : TermElabM Unit := do
  let goal ← mkFreshExprMVar (some (mkConst ``True))
  match ← OATP.Proof.reconstruct goal.mvarId! .trueIntro with
  | .ok _ => logInfo "OATP proof demo: kernel-checked True introduction"
  | .error message => throwError message

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
