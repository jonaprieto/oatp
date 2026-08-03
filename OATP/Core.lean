/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
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
