/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

/-!
# OATP.Core: proof-artifact-first ATP domain model

This module is pure. It does not run processes, contact services, or render
terminal output. A prover result is deliberately not a Lean proof: callers
must distinguish an untrusted candidate from a kernel-accepted proof.
-/

namespace OATP

structure Prover where
  name : String
  version : Option String := none
  deriving BEq, DecidableEq, Repr

namespace Prover

def label (prover : Prover) : String :=
  match prover.version with
  | some version => s!"{prover.name} {version}"
  | none => prover.name

end Prover

inductive SZSStatus where
  | theorem
  | unsatisfiable
  | satisfiable
  | counterSatisfiable
  | timeout
  | gaveUp
  | error
  | unknown
  deriving BEq, DecidableEq, Repr

namespace SZSStatus

private def statusPrefixes : List String := ["% SZS status ", "# SZS status "]

def tokenFromLine (line : String) : Option String :=
  let line := line.trimAscii.toString
  let rec find : List String → Option String
    | [] => none
    | marker :: markers =>
        if line.startsWith marker then
          let token := (line.drop marker.length).toString.splitOn " " |>.headD ""
          if token.isEmpty then none else some token
        else find markers
  find statusPrefixes

private def tokenFromLines : List String → Option String
  | [] => none
  | line :: lines => tokenFromLine line |>.orElse (fun _ => tokenFromLines lines)

private def resultTokenFromLine (line : String) : Option String :=
  let line := line.trimAscii.toString
  match line.splitOn " says " with
  | _ :: result :: _ =>
      let token := result.splitOn " " |>.headD ""
      if token.isEmpty then none else some token
  | _ => none

def tokenFromOutput (output : String) : Option String :=
  let lines := output.splitOn "\n"
  tokenFromLines lines |>.orElse fun _ =>
    lines.findSome? resultTokenFromLine

def ofString : String → Option SZSStatus
  | "Theorem" | "theorem" => some .theorem
  | "Unsatisfiable" | "unsatisfiable" => some .unsatisfiable
  | "Satisfiable" | "satisfiable" => some .satisfiable
  | "CounterSatisfiable" | "counterSatisfiable" | "countersatisfiable" =>
      some .counterSatisfiable
  | "Timeout" | "timeout" => some .timeout
  | "GaveUp" | "gaveUp" | "gaveup" => some .gaveUp
  | "Error" | "error" => some .error
  | "Unknown" | "unknown" => some .unknown
  | _ => none

def ofOutput (output : String) : Option SZSStatus :=
  tokenFromOutput output >>= ofString

def toString : SZSStatus → String
  | .theorem => "Theorem"
  | .unsatisfiable => "Unsatisfiable"
  | .satisfiable => "Satisfiable"
  | .counterSatisfiable => "CounterSatisfiable"
  | .timeout => "Timeout"
  | .gaveUp => "GaveUp"
  | .error => "Error"
  | .unknown => "Unknown"

instance : ToString SZSStatus where
  toString := toString

end SZSStatus

structure Limits where
  wallSeconds : Nat := 30
  maxOutputBytes : Nat := 4 * 1024 * 1024
  deriving BEq, DecidableEq, Repr

structure Problem where
  name : String
  source : String
  deriving BEq, DecidableEq, Repr

structure Artifact where
  prover : Prover
  status : SZSStatus
  problemName : Option String := none
  stdout : String := ""
  stderr : String := ""
  exitCode : Option UInt32 := none
  elapsedMs : Nat := 0
  deriving BEq, DecidableEq, Repr

inductive Outcome where
  | candidate (artifact : Artifact)
  | timedOut (artifact : Artifact)
  | failed (message : String) (artifact : Option Artifact := none)
  deriving Repr

def Outcome.artifact : Outcome → Option Artifact
  | .candidate artifact => some artifact
  | .timedOut artifact => some artifact
  | .failed _ artifact => artifact

def Outcome.status : Outcome → String
  | .candidate artifact => s!"candidate ({artifact.status})"
  | .timedOut _ => "timeout"
  | .failed message _ => s!"failed: {message}"

end OATP
