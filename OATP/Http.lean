/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core

/-!
# OATP.Http: command-backed HTTP prototype

Lean 4's `Std.Http` is the long-term protocol foundation. This first OATP
slice uses `curl` as the preferred explicit HTTPS/TLS transport and `wget` as a
fallback so the CLI can run on minimal systems. Arguments are passed as an argv
array; no shell is involved. Request bodies are checked before transport; the
response-size check is performed after capture; streaming cancellation remains
a follow-up.
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
    unless !part.value.contains boundary do
      throw s!"multipart field `{part.name}` contains the boundary"
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
  maxSeconds : Nat := OATP.defaultTimeoutSeconds
  maxBodyBytes : Nat := OATP.defaultMaxOutputBytes
  maxRequestBodyBytes : Nat := OATP.defaultMaxOutputBytes
  deriving Repr

structure Response where
  statusCode : Nat
  body : String
  stderr : String := ""
  deriving Repr

def commandVersion (command : String) : IO (Option String) := do
  try
    let output ← IO.Process.output { cmd := command, args := #["--version"] }
    if output.exitCode != 0 then
      pure none
    else
      let text := if output.stdout.isEmpty then output.stderr else output.stdout
      let line := text.splitOn "\n" |>.headD "" |>.trimAscii.toString
      pure <| if line.isEmpty then none else some line
  catch _ => pure none

def availableTransports : IO (Array String) := do
  let mut available := #[]
  for command in #["curl", "wget"] do
    if (← commandVersion command).isSome then
      available := available.push command
  pure available

inductive Error where
  | io (message : String)
  | invalidRequest (message : String)
  | transport (message : String)
  | malformedStatus (output : String)
  | requestBodyTooLarge (actual limit : Nat)
  | bodyTooLarge (actual limit : Nat)
  deriving Repr

inductive Transport where
  | curl
  | wget
  deriving BEq, DecidableEq, Repr

def statusMarker := "OATP_HTTP_STATUS:"

private def statusFromOutput (output : String) : Option (String × Nat) :=
  let parts := output.splitOn statusMarker
  match parts.reverse with
  | code :: before :: _ =>
      match code.toNat? with
      | some status => some (before, status)
      | none => none
  | _ => none

private def requestWithCurlUnsafe (request : Request) : IO (Except Error Response) := do
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

private def statusFromWgetLine (line : String) : Option Nat :=
  let fields := line.trimAscii.toString.splitOn " " |>.filter (!·.isEmpty)
  match fields with
  | _ :: code :: _ => code.toNat?
  | _ => none

private def statusFromWgetOutput (output : String) : Option Nat :=
  let rec find : List String → Option Nat
    | [] => none
    | line :: lines =>
        if line.trimAscii.toString.startsWith "HTTP/" then
          match statusFromWgetLine line with
          | some code => some code
          | none => find lines
        else find lines
  find (output.splitOn "\n")

private def requestWithWgetUnsafe (request : Request) : IO (Except Error Response) := do
  let base : Array String := #[
    "--quiet", "--server-response", "--max-redirect=0", "--tries=1",
    "--timeout=" ++ toString request.maxSeconds,
    "--quota=" ++ toString request.maxBodyBytes,
    "--output-document=-"
  ]
  let withHeaders := request.headers.foldl (fun args header =>
    args.push ("--header=" ++ header)) base
  let withBody := if request.method == .post then
    withHeaders.push ("--post-data=" ++ request.body)
  else
    withHeaders
  let output ← IO.Process.output {
    cmd := "wget"
    args := withBody.push request.url
  }
  match statusFromWgetOutput output.stderr with
  | none => return Except.error (.transport output.stderr)
  | some statusCode =>
      let actual := output.stdout.toUTF8.size
      if actual > request.maxBodyBytes then
        return Except.error (.bodyTooLarge actual request.maxBodyBytes)
      else
        return Except.ok { statusCode, body := output.stdout }

private def validateRequest (request : Request) : Except Error Unit := do
  if request.url.isEmpty then
    throw (.invalidRequest "HTTP request URL must not be empty")
  if request.maxSeconds == 0 then
    throw (.invalidRequest "HTTP request timeout must be greater than zero")
  if request.maxBodyBytes == 0 then
    throw (.invalidRequest "HTTP response limit must be greater than zero")
  let actual := request.body.toUTF8.size
  if actual > request.maxRequestBodyBytes then
    throw (.requestBodyTooLarge actual request.maxRequestBodyBytes)

private def requestWithTransportUnsafe (request : Request) : IO (Except Error Response) := do
  match validateRequest request with
  | .error error => return .error error
  | .ok _ => pure ()
  let available ← availableTransports
  if available.contains "curl" then
    requestWithCurlUnsafe request
  else if available.contains "wget" then
    requestWithWgetUnsafe request
  else
    pure <| Except.error (.transport "OATP requires curl or wget for HTTPS transport")

def requestWithTransport (request : Request) : IO (Except Error Response) := do
  try
    requestWithTransportUnsafe request
  catch error =>
    pure (.error (.io s!"{error}"))

def requestWith (transport : Transport) (request : Request) : IO (Except Error Response) := do
  try
    match validateRequest request with
    | .error error => pure (.error error)
    | .ok _ =>
        match transport with
        | .curl => requestWithCurlUnsafe request
        | .wget => requestWithWgetUnsafe request
  catch error =>
    pure (.error (.io s!"{error}"))

def requestWithCurl (request : Request) : IO (Except Error Response) := do
  requestWith .curl request

end OATP.Http
