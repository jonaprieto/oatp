/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core
import OATP.Artifacts
import OATP.Http
import Grip

/-!
# OATP.SystemOnTPTP: typed form adapter

The adapter preserves the old service's field names at one boundary. The rest
of OATP deals in typed requests and raw responses, so a change in the service
form does not leak into the core model.
-/

namespace OATP.SystemOnTPTP

open OATP
open OATP.Http.Form

def defaultCatalogueEndpoint : String := "https://tptp.org/cgi-bin/SystemOnTPTP"

def defaultEndpoint : String := "https://tptp.org/cgi-bin/SystemOnTPTPFormReply"

def defaultCatalogueTimeoutSeconds : Nat := OATP.defaultTimeoutSeconds

def defaultCatalogueMaxBodyBytes : Nat := 8 * 1024 * 1024

def defaultSystemTimeLimit : Nat := 60

def requestOverheadSeconds : Nat := 10

def onlineReferencePrefix : String := "online-"

def isOnlineReference (reference : String) : Bool := reference.startsWith onlineReferencePrefix

def onlineReference (systemId : String) : String := onlineReferencePrefix ++ systemId

def onlineSystemId (reference : String) : String :=
  if isOnlineReference reference then
    (reference.drop onlineReferencePrefix.length).toString
  else reference

structure Config where
  endpoint : String := defaultEndpoint
  systemLabel : String
  systemLabels : Array String := #[]
  systemCommands : Array (String × String) := #[]
  timeLimit : Nat := OATP.defaultTimeoutSeconds
  maxBodyBytes : Nat := OATP.defaultMaxOutputBytes
  deriving BEq, DecidableEq, Repr

namespace Catalogue

structure SystemInfo where
  id : String
  command : String := ""
  timeLimit : Nat := defaultSystemTimeLimit
  deriving BEq, DecidableEq, Repr

private def quotedAfter (marker line : String) : Option String :=
  match line.splitOn marker with
  | _ :: value :: _ => some (value.splitOn "\"" |>.headD "")
  | _ => none

private def systemId (line : String) : Option String :=
  (quotedAfter "NAME=\"System___" line).orElse fun _ =>
    quotedAfter "name=\"System___" line

private def fieldValue (field id current line : String) : String :=
  let lower := quotedAfter s!"name=\"{field}___{id}\"" line
  let upper := quotedAfter s!"NAME=\"{field}___{id}\"" line
  if lower.isSome || upper.isSome then
    (quotedAfter "value=\"" line).getD current
  else current

private def updateInfo (line : String) (system : SystemInfo) : SystemInfo :=
  let command := fieldValue "Command" system.id system.command line
  let timeLimit := (fieldValue "TimeLimit" system.id (toString system.timeLimit) line).toNat?.getD
    system.timeLimit
  { system with command, timeLimit }

def parse (html : String) : Array SystemInfo :=
  html.splitOn "\n" |>.foldl (init := #[]) fun systems line =>
    let systems := systems.map (updateInfo line)
    match systemId line with
    | some id => if id.isEmpty || systems.any (·.id == id) then systems else systems.push { id }
    | none => systems

def baseName (id : String) : String := id.splitOn "---" |>.headD id

def matchesReference (reference : String) (system : SystemInfo) : Bool :=
  let reference := onlineSystemId reference
  let wanted := reference.toLower
  let id := system.id.toLower
  id == wanted || (baseName system.id).toLower == wanted ||
    id.startsWith (wanted ++ "---")

def resolve (systems : Array SystemInfo) (reference : String) : Option SystemInfo :=
  systems.toList.find? (matchesReference reference)

end Catalogue

def labels (config : Config) : Array String :=
  if config.systemLabels.isEmpty then #[config.systemLabel] else config.systemLabels

private def command (config : Config) (label : String) : String :=
  config.systemCommands.toList.find? (·.1 == label) |>.map (·.2) |>.getD "default"

inductive ResponseError where
  | httpStatus (statusCode : Nat)
  | missingStatus
  | unsupportedStatus (value : String)
  | malformedBody (message : String)
  deriving Repr

def ResponseError.message : ResponseError → String
  | .httpStatus status => s!"SystemOnTPTP returned HTTP {status}"
  | .missingStatus => "SystemOnTPTP response did not contain an SZS status"
  | .unsupportedStatus status => s!"SystemOnTPTP returned unsupported SZS status `{status}`"
  | .malformedBody message => s!"SystemOnTPTP response was not valid HTML: {message}"

def fields (config : Config) (problem : Problem) : Array Field :=
  let base : Array Field := #[
    { name := "ProblemSource", value := "FORMULAE" },
    { name := "FORMULAEProblem", value := problem.source },
    { name := "QuietFlag", value := "-q01" },
    { name := "AutoMode", value := "-cU" },
    { name := "Intention", value := "THM" },
    { name := "SubmitButton", value := "RunSelectedSystems" },
  ]
  (labels config).foldl (fun fields label => fields ++ #[
    { name := s!"System___{label}", value := label },
    { name := s!"Command___{label}", value := command config label },
    { name := s!"Format___{label}", value := "tptp:raw" },
    { name := s!"TimeLimit___{label}", value := toString config.timeLimit },
    { name := s!"Transform___{label}", value := "none" }
  ]) base

def request (config : Config) (problem : Problem) : Http.Request where
  method := .post
  url := config.endpoint
  body := encodeUrlEncoded (fields config problem)
  headers := #["Content-Type: application/x-www-form-urlencoded"]
  maxSeconds := config.timeLimit + requestOverheadSeconds
  maxBodyBytes := config.maxBodyBytes
  maxRequestBodyBytes := config.maxBodyBytes

def submit (config : Config) (problem : Problem) :
  IO (Except Http.Error Http.Response) := do
  let label := String.intercalate ", " (labels config).toList
  let request := request config problem
  let artifacts ← OATP.Artifacts.start s!"online {label}" problem
  for run in artifacts do
    OATP.Artifacts.write run "request.txt" request.body
  let response ← Http.requestWithTransport request
  for run in artifacts do
    match response with
    | .ok response => do
        OATP.Artifacts.write run "response.html" response.body
        OATP.Artifacts.write run "response.meta"
          s!"status: {response.statusCode}\nstderr: {response.stderr}"
    | .error error => do
        OATP.Artifacts.write run "response.error" (Http.Error.message error)
  pure response

private inductive HtmlPart where
  | tag (value : String)
  | text (value : String)

open Grip GParser

private def htmlTag : GParser conditional HtmlPart :=
  HtmlPart.tag <$> GParser.capture (GParser.ch '<' *> GParser.takeWhile (· != 62) <* GParser.ch '>')

private def htmlText : GParser conditional HtmlPart :=
  HtmlPart.text <$> GParser.capture (GParser.takeWhile1 (· != 60))

private def htmlPart : GParser conditional HtmlPart :=
  GParser.chooseG htmlTag [htmlText]

private def htmlDocument : Grip.Parser (List HtmlPart) :=
  GParser.seqL (GParser.many htmlPart) GParser.eof

private def tagStarts (needle : String) (tag : String) : Bool :=
  tag.toLower.startsWith needle

private def bodyText (parts : List HtmlPart) : String :=
  let hasBody := parts.any fun part => match part with
    | .tag tag => tagStarts "<body" tag
    | .text _ => false
  let rec collect (inside : Bool) : List HtmlPart → List String
    | [] => []
    | part :: rest =>
        match part with
        | .text value => if inside then value :: collect inside rest else collect inside rest
        | .tag tag =>
            let opening := tagStarts "<body" tag
            let closing := tagStarts "</body" tag
            let lineBreak := tagStarts "<br" tag || tagStarts "<pre" tag ||
              tagStarts "</pre" tag
            if closing then []
            else if opening then collect true rest
            else if inside && lineBreak then "\n" :: collect inside rest
            else collect inside rest
  String.intercalate "" (collect (!hasBody) parts)

private def decodeHtmlEntities (value : String) : String :=
  value.replace "&gt;" ">" |>.replace "&lt;" "<" |>.replace "&amp;" "&"
    |>.replace "&quot;" "\"" |>.replace "&#39;" "'"

def parseResponseText (body : String) : Except Grip.ParseError String :=
  let body := body.trimAscii.toString
  if !body.startsWith "<!DOCTYPE" && !body.toLower.startsWith "<html" then
    .ok body
  else
    match htmlDocument.parse body.toUTF8 with
    | .error error => .error error
    | .ok parts => .ok <| decodeHtmlEntities (bodyText parts).trimAscii.toString

def responseText (body : String) : String :=
  match parseResponseText body with
  | .ok text => text
  | .error _ => body

def catalogueRequest (endpoint : String) : Http.Request where
  method := .get
  url := if endpoint == defaultEndpoint then defaultCatalogueEndpoint else endpoint
  maxSeconds := defaultCatalogueTimeoutSeconds
  maxBodyBytes := defaultCatalogueMaxBodyBytes
  maxRequestBodyBytes := defaultCatalogueMaxBodyBytes

def fetchCatalogue (endpoint : String) : IO (Except Http.Error Http.Response) :=
  Http.requestWithTransport (catalogueRequest endpoint)

def parseResponse (config : Config) (problem : Problem) (response : Http.Response) :
    Except ResponseError Artifact := do
  if response.statusCode < 200 || response.statusCode ≥ 300 then
    throw (.httpStatus response.statusCode)
  let token ← match SZSStatus.tokenFromOutput response.body with
    | some token => Except.ok token
    | none => Except.error .missingStatus
  let status ← match SZSStatus.ofString token with
    | some status => Except.ok status
    | none => Except.error (.unsupportedStatus token)
  let stdout ← match parseResponseText response.body with
    | .ok text => Except.ok text
    | .error error => Except.error (.malformedBody error.message)
  pure {
    prover := { name := String.intercalate ", " (labels config).toList }
    status
    problemName := some problem.name
    stdout
    stderr := response.stderr
  }

end OATP.SystemOnTPTP
