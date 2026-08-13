/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core
import OATP.Artifacts

/-!
# OATP.Process: local prover execution

Commands are passed as argv to Lean's native process API. The runner sends the
problem on stdin, drains both output streams concurrently, requests process
group termination on timeout, and applies the output limit after capture.
-/

namespace OATP.Process

structure Command where
  executable : String
  arguments : Array String := #[]
  cwd : Option System.FilePath := none
  deriving BEq, DecidableEq, Repr

/-- Add the stdin filename expected by local provers that require one. -/
def Command.withStdinProblem (command : Command) : Command :=
  let executable := (command.executable.splitOn "/").getLast?.getD command.executable
  if executable == "metis" && !command.arguments.any (· == "-") then
    { command with arguments := command.arguments.push "-" }
  else command

inductive Error where
  | io (message : String)
  | outputTooLarge (actual limit : Nat)
  deriving Repr

-- partiality: process completion and the monotonic clock are external; the deadline bounds
-- polling behavior, not kernel recursion.
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

private def runUnsafe (prover : Prover) (problem : Problem) (limits : Limits) (command : Command) :
    IO (Except Error Artifact) := do
  let command := command.withStdinProblem
  let artifacts ← OATP.Artifacts.start s!"local {command.executable}" problem
  for run in artifacts do
    OATP.Artifacts.writeCommand run
      (String.intercalate " " (command.executable :: command.arguments.toList))
  let child ← try
    IO.Process.spawn {
      cmd := command.executable
      args := command.arguments
      cwd := command.cwd
      stdin := .piped
      stdout := .piped
      stderr := .piped
      setsid := true
    }
  catch error => do
    for run in artifacts do
      OATP.Artifacts.write run "error.txt" s!"{error}"
      OATP.Artifacts.write run "result.txt" "error\n"
    return .error (.io s!"{error}")
  let (stdin, child) ← child.takeStdin
  let stdoutTask ← IO.asTask child.stdout.readToEnd Task.Priority.dedicated
  let stderrTask ← IO.asTask child.stderr.readToEnd Task.Priority.dedicated
  let started ← IO.monoMsNow
  let stdinTask ← IO.asTask (do
    stdin.putStr problem.source
    stdin.flush) Task.Priority.dedicated
  let (timedOut, exitCode) ← waitForExit child (started + limits.wallSeconds * 1000)
  let _ ← IO.ofExcept stdinTask.get
  let stdout ← IO.ofExcept stdoutTask.get
  let stderr ← IO.ofExcept stderrTask.get
  for run in artifacts do
    OATP.Artifacts.write run "stdout.txt" stdout
    OATP.Artifacts.write run "stderr.txt" stderr
  if stderr.startsWith "could not execute external process" then
    let message := stderr.trimAscii.toString
    for run in artifacts do
      OATP.Artifacts.write run "error.txt" message
      OATP.Artifacts.write run "result.txt" "error\n"
    return .error (.io message)
  let elapsedMs := (← IO.monoMsNow) - started
  let actual := stdout.toUTF8.size + stderr.toUTF8.size
  if actual > limits.maxOutputBytes then
    let message :=
      s!"local prover output exceeded {limits.maxOutputBytes} bytes ({actual} captured)"
    for run in artifacts do
      OATP.Artifacts.write run "error.txt" message
      OATP.Artifacts.write run "result.txt" "error\n"
    return .error (.outputTooLarge actual limits.maxOutputBytes)
  let status := if timedOut then .timeout else if exitCode == 0 then
      (SZSStatus.ofOutput stdout).getD .unknown
    else .error
  let artifact : Artifact := {
    prover,
    status,
    problemName := some problem.name,
    stdout,
    stderr,
    exitCode := some exitCode,
    elapsedMs
  }
  for run in artifacts do
    OATP.Artifacts.write run "result.txt" s!"{status}\n"
  return .ok artifact

def run (prover : Prover) (problem : Problem) (limits : Limits) (command : Command) :
    IO (Except Error Artifact) := do
  try
    runUnsafe prover problem limits command
  catch error =>
    pure (.error (.io s!"{error}"))

end OATP.Process
