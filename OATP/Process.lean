/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core

/-!
# OATP.Process: local prover execution

Commands are passed as argv to Lean's native process API. The runner sends the
problem on stdin, drains both output streams concurrently, kills the process
group on timeout, and applies the output limit after capture.
-/

namespace OATP.Process

structure Command where
  executable : String
  arguments : Array String := #[]
  cwd : Option System.FilePath := none
  deriving BEq, DecidableEq, Repr

inductive Error where
  | timedOut
  | outputTooLarge (actual limit : Nat)
  deriving Repr

private partial def waitForExit {cfg : IO.Process.StdioConfig}
    (child : IO.Process.Child cfg) (deadline : Nat) : IO (Bool × UInt32) := do
  match ← child.tryWait with
  | some exitCode => pure (false, exitCode)
  | none =>
      let now ← IO.monoMsNow
      if now ≥ deadline then
        child.kill
        let exitCode ← child.wait
        pure (true, exitCode)
      else
        let remaining := deadline - now
        IO.sleep (min 10 remaining).toUInt32
        waitForExit child deadline

def run (prover : Prover) (problem : Problem) (limits : Limits) (command : Command) :
    IO (Except Error Artifact) := do
  let child ← IO.Process.spawn {
    cmd := command.executable
    args := command.arguments
    cwd := command.cwd
    stdin := .piped
    stdout := .piped
    stderr := .piped
    setsid := true
  }
  let (stdin, child) ← child.takeStdin
  stdin.putStr problem.source
  stdin.flush
  let stdoutTask ← IO.asTask child.stdout.readToEnd Task.Priority.dedicated
  let stderrTask ← IO.asTask child.stderr.readToEnd Task.Priority.dedicated
  let started ← IO.monoMsNow
  let (timedOut, exitCode) ← waitForExit child (started + limits.wallSeconds * 1000)
  let stdout ← IO.ofExcept stdoutTask.get
  let stderr ← IO.ofExcept stderrTask.get
  let elapsedMs := (← IO.monoMsNow) - started
  let actual := stdout.toUTF8.size + stderr.toUTF8.size
  if actual > limits.maxOutputBytes then
    return .error (.outputTooLarge actual limits.maxOutputBytes)
  let status := if timedOut then .timeout else if exitCode == 0 then .unknown else .error
  let artifact : Artifact := {
    prover,
    status,
    stdout,
    stderr,
    exitCode := some exitCode,
    elapsedMs
  }
  return .ok artifact

end OATP.Process
