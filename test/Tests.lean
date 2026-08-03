/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP
import OATP.TPTP
import OATP.SystemOnTPTP
import OATP.Term

open OATP OATP.TPTP

#guard SZSStatus.toString .theorem == "Theorem"
#guard (parseStatement "fof(goal, conjecture, p)." |>.isOk)
#guard (parseStatement "cnf(c1, axiom, p | ~q)." |>.isOk)
#guard match parseStatement "fof(goal, conjecture, p(f(a,b)))." with
  | .ok statement => statement.formula == "p(f(a,b))" && statement.annotations.isNone
  | .error _ => false
#guard match parseStatement "fof(goal, conjecture, p('x)'), status(thm, theorem))." with
  | .ok statement => statement.formula == "p('x)')" &&
      statement.annotations == some "status(thm, theorem)"
  | .error _ => false
#guard match parseStatement "not-tptp" with
  | .error _ => true
  | .ok _ => false
#guard match parseStatement "fof(goal, conjecture, p). trailing" with
  | .error _ => true
  | .ok _ => false
#guard match OATP.SystemOnTPTP.parseResponse
    { systemLabel := "vampire" }
    { statusCode := 200, body := "% SZS status Theorem for goal\n" } with
  | .ok artifact => artifact.status == .theorem && artifact.prover.name == "vampire"
  | .error _ => false
#guard match OATP.SystemOnTPTP.parseResponse
    { systemLabel := "vampire" }
    { statusCode := 500, body := "server error" } with
  | .error (.httpStatus 500) => true
  | _ => false
#guard OATP.SystemOnTPTP.encodeComponent "a b&c" == "a%20b%26c"
#guard OATP.SystemOnTPTP.encodeForm #[
  { name := "x", value := "a b" },
  { name := "y", value := "✓" }
] == "x=a%20b&y=%E2%9C%93"
#guard OATP.Term.renderPlain #[.goal {
  title := "demo"
  context := #["h : p"]
  target := "p"
}] == "goal: demo\n  h : p\n⊢ p"

def main : IO UInt32 := do
  let problem : Problem := { name := "stdin", source := "fof(goal, conjecture, p).\n" }
  let processResult ← OATP.Process.run
    { name := "cat" } problem { wallSeconds := 2 }
    { executable := "cat" }
  match processResult with
  | .ok artifact =>
      if artifact.stdout != problem.source then
        throw <| IO.userError "local process backend did not preserve stdin"
  | .error _ =>
      throw <| IO.userError "local process backend failed to run cat"
  let limited ← OATP.Process.run
    { name := "cat" } problem { wallSeconds := 2, maxOutputBytes := 1 }
    { executable := "cat" }
  match limited with
  | .error (.outputTooLarge actual 1) =>
      if actual ≤ 1 then
        throw <| IO.userError "local process backend reported an invalid output size"
  | _ =>
      throw <| IO.userError "local process backend ignored the output limit"
  IO.println "OATP tests passed"
  return 0
