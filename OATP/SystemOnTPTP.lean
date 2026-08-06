/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core
import OATP.Http

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

structure Config where
  endpoint : String := defaultEndpoint
  systemLabel : String
  systemLabels : Array String := #[]
  systemCommands : Array (String × String) := #[]
  timeLimit : Nat := 30
  maxBodyBytes : Nat := 4 * 1024 * 1024
  deriving BEq, DecidableEq, Repr

namespace Catalogue

structure SystemInfo where
  id : String
  command : String := ""
  timeLimit : Nat := 60
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
  let reference := if reference.startsWith "online-" then
      (reference.drop "online-".length).toString
    else reference
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
  deriving Repr

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
  maxSeconds := config.timeLimit + 10
  maxBodyBytes := config.maxBodyBytes
  maxRequestBodyBytes := config.maxBodyBytes

def submit (config : Config) (problem : Problem) :
  IO (Except Http.Error Http.Response) :=
  Http.requestWithTransport (request config problem)

def catalogueRequest (endpoint : String) : Http.Request where
  method := .get
  url := if endpoint == defaultEndpoint then defaultCatalogueEndpoint else endpoint
  maxSeconds := 30
  maxBodyBytes := 8 * 1024 * 1024
  maxRequestBodyBytes := 8 * 1024 * 1024

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
  pure {
    prover := { name := String.intercalate ", " (labels config).toList }
    status
    problemName := some problem.name
    stdout := response.body
    stderr := response.stderr
  }

end OATP.SystemOnTPTP
