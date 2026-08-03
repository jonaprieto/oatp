/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
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

namespace Form

structure Field where
  name : String
  value : String
  deriving BEq, DecidableEq, Repr

private def hexDigits : Array Char :=
  #[
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  ]

private def isUnreserved (byte : UInt8) : Bool :=
  (byte >= 48 && byte <= 57) ||
  (byte >= 65 && byte <= 90) ||
  (byte >= 97 && byte <= 122) ||
  byte == 45 || byte == 46 || byte == 95 || byte == 126

private def encodeByte (byte : UInt8) : String :=
  if isUnreserved byte then
    Char.ofNat byte.toNat |>.toString
  else
    let n := byte.toNat
    s!"%{hexDigits[n / 16]!}{hexDigits[n % 16]!}"

def encodeComponent (value : String) : String :=
  String.join (value.toUTF8.toList.map encodeByte)

def encodeUrlEncoded (fields : Array Field) : String :=
  String.intercalate "&" <| fields.toList.map fun field =>
    s!"{encodeComponent field.name}={encodeComponent field.value}"

structure MultipartPart where
  name : String
  value : String
  filename : Option String := none
  contentType : Option String := none
  deriving BEq, DecidableEq, Repr

private def validHeaderValue (value : String) : Bool :=
  value.toList.all fun character =>
    character != '"' && character != '\\' && character != '\r' && character != '\n'

def encodeMultipart (boundary : String) (parts : Array MultipartPart) : Except String String := do
  if boundary.isEmpty || !boundary.toList.all (fun character =>
      character.isAlphanum || character == '-' || character == '_') then
    throw "multipart boundary must contain only letters, digits, '-' or '_'"
  for part in parts do
    unless validHeaderValue part.name do
      throw s!"invalid multipart field name `{part.name}`"
    for filename in part.filename do
      unless validHeaderValue filename do
        throw s!"invalid multipart filename `{filename}`"
    for contentType in part.contentType do
      unless validHeaderValue contentType do
        throw s!"invalid multipart content type `{contentType}`"
  let encoded := parts.toList.map fun part =>
    let disposition := match part.filename with
      | some filename => s!"; filename=\"{filename}\""
      | none => ""
    let contentType := part.contentType.map (fun value => s!"\r\nContent-Type: {value}") |>.getD ""
    let headers := s!"--{boundary}\r\nContent-Disposition: form-data; name=\"{part.name}\"" ++
      disposition ++ contentType
    s!"{headers}\r\n\r\n{part.value}\r\n"
  pure <| String.join encoded ++ s!"--{boundary}--\r\n"

end Form

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
