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
    { name := "goal", source := "fof(goal, conjecture, p)." }
    { statusCode := 200, body := "% SZS status Theorem for goal\n" } with
  | .ok artifact => artifact.status == .theorem && artifact.prover.name == "vampire" &&
      artifact.problemName == some "goal"
  | .error _ => false
#guard match OATP.SystemOnTPTP.parseResponse
    { systemLabel := "vampire" }
    { name := "goal", source := "" }
    { statusCode := 500, body := "server error" } with
  | .error (.httpStatus 500) => true
  | _ => false
#guard OATP.Http.Form.encodeComponent "a b&c" == "a%20b%26c"
#guard OATP.Http.Form.encodeUrlEncoded #[
  { name := "x", value := "a b" },
  { name := "y", value := "✓" }
] == "x=a%20b&y=%E2%9C%93"
#guard match OATP.Http.Form.encodeMultipart "oatp-boundary" #[
    { name := "problem", value := "fof(goal, conjecture, p)." }
  ] with
  | .ok body => body.startsWith "--oatp-boundary\r\n"
  | .error _ => false
#guard match OATP.Http.Form.encodeMultipart "bad\r\n" #[] with
  | .error _ => true
  | .ok _ => false
#guard match OATP.Http.Form.encodeMultipart "boundary" #[
    { name := "problem", value := "contains-boundary" }
  ] with
  | .error _ => true
  | .ok _ => false
#guard OATP.Term.renderPlain #[.goal {
  title := "demo"
  context := #["h : p"]
  target := "p"
}] == "goal: demo\n  h : p\n⊢ p"
def main : IO UInt32 := do
  let x := OATP.TPTP.Syntax.Term.function "f" #[
    .constant "a", .var "X"
  ]
  let formula := OATP.TPTP.Syntax.Formula.forall "X"
    (.implies (.atom "p" #[x]) (.atom "q" #[.var "X"]))
  let rendered ← match formula.toTPTP with
    | .ok rendered => pure rendered
    | .error message => throw <| IO.userError message
  if rendered != "![X] : ((p(f(a, X)) => q(X)))" then
    throw <| IO.userError "first-order formula rendering changed"
  match Statement.ofFof "goal" .conjecture formula with
  | .ok statement =>
      if statement.formula != rendered then
        throw <| IO.userError "first-order statement rendering changed"
  | .error message => throw <| IO.userError message
  let invalid := OATP.TPTP.Syntax.Formula.atom "Bad" #[]
  match invalid.toTPTP with
  | .error _ => pure ()
  | .ok _ => throw <| IO.userError "invalid TPTP symbol was rendered"
  let unbound := OATP.TPTP.Syntax.Formula.atom "p" #[.var "X"]
  match unbound.toTPTP with
  | .error _ => pure ()
  | .ok _ => throw <| IO.userError "unbound TPTP variable was rendered"
  let theoremFixture ← IO.FS.readFile "test/fixtures/system-on-tptp/theorem.txt"
  let theoremArtifact ← match OATP.SystemOnTPTP.parseResponse
      { systemLabel := "vampire" } { name := "fixture", source := "" }
      { statusCode := 200, body := theoremFixture } with
    | .ok artifact => pure artifact
    | .error _ => throw <| IO.userError "theorem fixture did not parse"
  if theoremArtifact.status != .theorem || theoremArtifact.problemName != some "fixture" then
    throw <| IO.userError "theorem fixture metadata changed"
  let timeoutFixture ← IO.FS.readFile "test/fixtures/system-on-tptp/timeout.txt"
  let timeoutArtifact ← match OATP.SystemOnTPTP.parseResponse
      { systemLabel := "vampire" } { name := "fixture", source := "" }
      { statusCode := 200, body := timeoutFixture } with
    | .ok artifact => pure artifact
    | .error _ => throw <| IO.userError "timeout fixture did not parse"
  if timeoutArtifact.status != .timeout then
    throw <| IO.userError "timeout fixture status changed"
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
  let largeProblem : Problem := {
    name := "large-stdin"
    source := String.join (List.replicate 200000 "x")
  }
  let largeResult ← OATP.Process.run
    { name := "cat" } largeProblem { wallSeconds := 2 }
    { executable := "cat" }
  match largeResult with
  | .ok artifact =>
      if artifact.stdout != largeProblem.source then
        throw <| IO.userError "local process backend deadlocked on large stdin"
  | .error _ =>
      throw <| IO.userError "local process backend rejected large stdin"
  let missing ← OATP.Process.run
    { name := "missing" } problem { wallSeconds := 2 }
    { executable := "oatp-executable-that-does-not-exist" }
  match missing with
  | .ok artifact =>
      if artifact.status != .error then
        throw <| IO.userError "missing executable was not reported as a process error"
  | .error (.io _) => pure ()
  | .error _ => throw <| IO.userError "local process IO failure was misclassified"
  IO.println "OATP tests passed"
  return 0
