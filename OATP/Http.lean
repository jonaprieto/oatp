/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# OATP.Http: command-backed HTTP prototype

Lean 4's `Std.Http` is the long-term protocol foundation. This first OATP
slice uses `curl` as an explicit HTTPS/TLS transport so the service adapter can
be tested and shipped before OATP requires a new TLS implementation. Arguments
are passed as an argv array; no shell is involved. The response-size check is
performed after capture; streaming cancellation remains a follow-up.
-/

namespace OATP.Http

inductive Method where
  | get
  | post
  deriving BEq, DecidableEq, Repr

def Method.toString : Method → String
  | .get => "GET"
  | .post => "POST"

structure Request where
  method : Method := .get
  url : String
  body : String := ""
  headers : Array String := #[]
  maxSeconds : Nat := 30
  maxBodyBytes : Nat := 4 * 1024 * 1024
  deriving Repr

structure Response where
  statusCode : Nat
  body : String
  stderr : String := ""
  deriving Repr

inductive Error where
  | transport (message : String)
  | malformedStatus (output : String)
  | bodyTooLarge (actual limit : Nat)
  deriving Repr

def statusMarker := "OATP_HTTP_STATUS:"

private def statusFromOutput (output : String) : Option (String × Nat) :=
  let parts := output.splitOn statusMarker
  match parts.reverse with
  | code :: before :: _ =>
      match code.toNat? with
      | some status => some (before, status)
      | none => none
  | _ => none

def requestWithCurl (request : Request) : IO (Except Error Response) := do
  let base : Array String := #[
    "--silent", "--show-error",
    "--max-time", toString request.maxSeconds,
    "--max-filesize", toString request.maxBodyBytes,
    "--write-out", "\\n" ++ statusMarker ++ "%{http_code}",
    "--request", request.method.toString]
  let withHeaders := request.headers.foldl (fun args header =>
    args.push "--header" |>.push header) base
  let withBody := if request.method == .post then
    withHeaders.push "--data-raw" |>.push request.body
  else
    withHeaders
  let output ← IO.Process.output {
    cmd := "curl"
    args := withBody.push request.url
  }
  if output.exitCode != 0 then
    return Except.error (Error.transport output.stderr)
  match statusFromOutput output.stdout with
  | none => return Except.error (Error.malformedStatus output.stdout)
  | some (body, statusCode) =>
      let actual := body.toUTF8.size
      if actual > request.maxBodyBytes then
        return Except.error (Error.bodyTooLarge actual request.maxBodyBytes)
      else
        return Except.ok { statusCode := statusCode, body := body, stderr := output.stderr }

end OATP.Http
