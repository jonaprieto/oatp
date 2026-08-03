/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP
import OATP.Term

open OATP

def sampleEvents : Array SearchEvent := #[
  .goal { title := "demo theorem", target := "a ≤ c", context := #["h₁ : a ≤ b", "h₂ : b ≤ c"] },
  .attempt { tactic := "simp_all", outcome := "no progress", elapsedMs := 1 },
  .attempt { tactic := "aesop", outcome := "candidate", elapsedMs := 4 },
  .note "external ATP results remain candidates until Lean reconstruction",
  .result (.candidate {
    prover := { name := "vampire", version := some "prototype" }
    status := .theorem
    elapsedMs := 8
  })]

def main : IO Unit := do
  IO.println (OATP.Term.renderPlain sampleEvents)
  IO.println ""
  IO.print (OATP.Term.renderAnsi16 sampleEvents)
