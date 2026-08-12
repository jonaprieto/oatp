/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Process
import OATP.SystemOnTPTP

/-!
# OATP.Portfolio: concurrent prover attempts

Local attempts are independent processes. Online attempts are represented as a
single SystemOnTPTP batch by the CLI, so the service can apply its own limits.
-/

namespace OATP.Portfolio

open OATP

inductive Backend where
  | local (command : Process.Command)
  | online (config : SystemOnTPTP.Config)
  deriving Repr

structure Attempt where
  name : String
  backend : Backend
  limits : Limits := {}
  deriving Repr

inductive Failure where
  | process (message : String)
  | http (error : Http.Error)
  | response (error : SystemOnTPTP.ResponseError) (output : String)
  deriving Repr

inductive Result where
  | artifact (attempt : Attempt) (value : Artifact)
  | failed (attempt : Attempt) (failure : Failure)
  deriving Repr

private def responseErrorMessage : SystemOnTPTP.ResponseError → String
  | .httpStatus status => s!"SystemOnTPTP returned HTTP {status}"
  | .missingStatus => "SystemOnTPTP response did not contain an SZS status"
  | .unsupportedStatus status => s!"SystemOnTPTP returned unsupported SZS status `{status}`"
  | .malformedBody message => s!"SystemOnTPTP response was not valid HTML: {message}"

private def httpErrorMessage : Http.Error → String
  | .io message => s!"HTTP IO failed: {message}"
  | .invalidRequest message => s!"invalid HTTP request: {message}"
  | .transport message => s!"HTTP transport failed: {message}"
  | .malformedStatus output => s!"HTTP response had no usable status: {output}"
  | .requestBodyTooLarge actual limit => s!"HTTP request exceeded {limit} bytes ({actual} captured)"
  | .bodyTooLarge actual limit => s!"HTTP response exceeded {limit} bytes ({actual} captured)"

def Failure.message : Failure → String
  | .process message => message
  | .http error => httpErrorMessage error
  | .response error _ => responseErrorMessage error

def Failure.output : Failure → String
  | .http (.malformedStatus output) => output
  | .process _ | .http _ => ""
  | .response _ output => output

def execute (problem : Problem) (attempt : Attempt) : IO Result := do
  match attempt.backend with
  | .local command =>
      match ← Process.run { name := attempt.name } problem attempt.limits command with
      | .ok artifact => pure (.artifact attempt artifact)
      | .error (.io message) => pure (.failed attempt (.process message))
      | .error (.outputTooLarge actual limit) =>
          pure <| .failed attempt <| .process
            s!"local prover output exceeded {limit} bytes ({actual} captured)"
  | .online config =>
      let started ← IO.monoMsNow
      match ← SystemOnTPTP.submit config problem with
      | .error error => pure (.failed attempt (.http error))
      | .ok response =>
          match SystemOnTPTP.parseResponse config problem response with
          | .ok artifact =>
              pure (.artifact attempt { artifact with elapsedMs := (← IO.monoMsNow) - started })
          | .error error =>
              let output := if response.stderr.isEmpty then response.body else
                response.body ++ "\n\nstderr:\n" ++ response.stderr
              pure (.failed attempt (.response error output))

-- partiality: task completion is external and waitAny' controls progress through the pending set.
private partial def collect (pending : List (Task (Except IO.Error Result)))
    (results : Array Result) (onResult : Result → IO Unit) : IO (Array Result) := do
  match pending with
  | [] => pure results
  | task :: rest =>
      let (result, remaining) ← IO.waitAny' (task :: rest)
      let result ← IO.ofExcept result
      onResult result
      collect remaining (results.push result) onResult

def runWith (problem : Problem) (attempts : Array Attempt)
    (onResult : Result → IO Unit := fun _ => pure ()) : IO (Array Result) := do
  let tasks ← attempts.toList.mapM fun attempt =>
    IO.asTask (execute problem attempt) Task.Priority.dedicated
  collect tasks #[] onResult

def run (problem : Problem) (attempts : Array Attempt) : IO (Array Result) :=
  runWith problem attempts

end OATP.Portfolio
