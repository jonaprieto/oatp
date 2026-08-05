/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Argus
import Argus.Term
import OATP
import TermColor.Diagnostics
import TermColor.Terminal

open Argus
open OATP
open TermColor
open TermColor.Diagnostics
open TermColor.Terminal
open scoped TermColor.Style

def cliVersion : String := "0.2.1"

argus_opts LocalOptions where
  executable : String := Spec.flag "executable" (some 'x') "Local prover executable" Param.path;
  problem : String := Spec.arg "PROBLEM" "TPTP problem file" Param.path;
  timeout : Option Nat := Spec.opt (Spec.flag "timeout" (some 't')
    "Wall-clock limit" Param.duration);
  maxOutput : Option Nat := Spec.opt (Spec.flag "max-output" none
    "Maximum captured output" Param.bytes);
  arguments : List String := Spec.many (Spec.arg "ARG" "Argument passed to the prover" Param.str)

argus_opts OnlineOptions where
  system : String := Spec.flag "system" (some 's') "SystemOnTPTP system label" Param.str;
  problem : String := Spec.arg "PROBLEM" "TPTP problem file" Param.path;
  endpoint : Option String := Spec.opt (Spec.flag "endpoint" none
    "SystemOnTPTP endpoint" Param.str);
  timeout : Option Nat := Spec.opt (Spec.flag "timeout" (some 't')
    "Remote time limit" Param.duration);
  maxOutput : Option Nat := Spec.opt (Spec.flag "max-output" none
    "Maximum captured response" Param.bytes)

inductive Action where
  | local (options : LocalOptions)
  | online (options : OnlineOptions)
  | doctor

def cli : Command Action :=
  Argus.group "oatp"
    [ Argus.cmd "local" (Spec.map Action.local LocalOptions.spec)
        (description := "Run a local ATP process")
    , Argus.cmd "online" (Spec.map Action.online OnlineOptions.spec)
        (description := "Submit a problem to SystemOnTPTP")
    , Argus.cmd "doctor" (Spec.const Action.doctor)
        (description := "Check local tools and transport readiness") ]
    (version := some cliVersion)
    (description := "Proof-artifact-first ATP orchestration")

private def doctorPalette : ColorScheme := ColorScheme.catppuccin

private def doctorSection (title : String) : IO Unit := do
  writeTextLine Text.empty
  writeTextLine (Text.styled title (Style.bold <+> Style.fg doctorPalette.purple))

private def doctorRow (label value : String) (ok : Bool) : IO Unit := do
  let marker := if ok then
      Text.styled "✓" (Style.bold <+> Style.fg doctorPalette.green)
    else
      Text.styled "·" (Style.bold <+> Style.fg doctorPalette.yellow)
  let label := Layout.padRight 10
    (Text.styled label (Style.bold <+> Style.fg doctorPalette.cyan))
  let valueStyle := if ok then Style.fg doctorPalette.foreground else Style.fg doctorPalette.yellow
  writeTextLine (Text.plain "  " ++ marker ++ Text.plain " " ++ label ++ Text.plain " " ++
    Text.styled value valueStyle)

private def doctorTool (command : String) : IO Unit := do
  match ← Http.commandVersion command with
  | some version => doctorRow command version true
  | none => doctorRow command "not found" false

private def doctorPlatform : IO String := do
  try
    let output ← IO.Process.output { cmd := "uname", args := #["-s", "-m"] }
    if output.exitCode == 0 then
      pure output.stdout.trimAscii.toString
    else
      pure "unavailable"
  catch _ => pure "unavailable"

private def runDoctor : IO UInt32 := do
  let transports ← Http.availableTransports
  let (online, ready) :=
    if transports.contains "curl" then
      ("ready (curl preferred)", true)
    else if transports.contains "wget" then
      ("ready (wget fallback)", true)
    else
      ("unavailable (install curl or wget)", false)
  writeTextLine (Text.styled s!"oatp doctor {cliVersion}"
    (Style.bold <+> Style.fg doctorPalette.purple))
  doctorSection "SYSTEM"
  doctorRow "platform" (← doctorPlatform) true

  doctorSection "TRANSPORT"
  for command in #["curl", "wget"] do
    doctorTool command
  doctorRow "online" online ready

  doctorSection "LOCAL ATP"
  for command in #["eprover", "vampire", "metis"] do
    doctorTool command

  doctorSection "OPTIONAL"
  doctorTool "docker"
  pure <| if transports.isEmpty then 1 else 0

private def printDiagnostic (message : String) : IO UInt32 := do
  let stderr ← IO.getStderr
  let target ← TermColor.targetWithTty .auto (← stderr.isTty)
  let diagnostic := TermColor.Diagnostics.render #[] (Diagnostic.error message)
  stderr.putStr (Text.render target diagnostic)
  stderr.putStr "\n"
  pure 1

private partial def progressLoop (finished : IO.Ref Bool)
    (live : LiveIndeterminateProgress) : IO Unit := do
  if ← finished.get then
    let _ ← live.finish
    pure ()
  else
    let live ← live.tick
    IO.sleep 120
    progressLoop finished live

private def withProgress {α : Type} (label : String) (action : IO α) : IO α := do
  if !(← stdoutSupportsControl) then
    return ← action
  withHiddenCursor do
    let finished ← IO.mkRef false
    let initial := LiveIndeterminateProgress.start {
      width := 24
      indeterminateWidth := 7
    }
    let initial := { initial with
      state := { initial.state with label := Text.plain label } }
    let progress ← IO.asTask (do
      progressLoop finished initial
      ) Task.Priority.dedicated
    let result ← try action finally
      finished.set true
      let _ ← IO.ofExcept progress.get
    pure result

private def readProblem (path : String) : IO Problem := do
  pure { name := path, source := ← IO.FS.readFile path }

private def showArtifact (artifact : Artifact) : IO UInt32 := do
  unless artifact.stdout.isEmpty do IO.print artifact.stdout
  unless artifact.stderr.isEmpty do IO.eprint artifact.stderr
  IO.eprintln s!"{artifact.prover.label}: {artifact.status} ({artifact.elapsedMs}ms)"
  pure <| if artifact.status == .theorem then 0 else 1

private def processErrorMessage : OATP.Process.Error → String
  | .io message => s!"local prover IO failed: {message}"
  | .outputTooLarge actual limit =>
      s!"local prover output exceeded {limit} bytes ({actual} captured)"

private def runLocal (options : LocalOptions) : IO UInt32 := do
  try
    let problem ← readProblem options.problem
    let limits : Limits := {
      wallSeconds := options.timeout.getD 30
      maxOutputBytes := options.maxOutput.getD (4 * 1024 * 1024)
    }
    let result ← withProgress s!"local {options.executable}" <| Process.run
      { name := options.executable }
      problem
      limits
      { executable := options.executable, arguments := options.arguments.toArray }
    match result with
    | .ok artifact => showArtifact artifact
    | .error error => printDiagnostic (processErrorMessage error)
  catch error => printDiagnostic s!"could not read problem: {error}"

private def httpErrorMessage : OATP.Http.Error → String
  | .io message => s!"HTTP IO failed: {message}"
  | .transport message => s!"HTTP transport failed: {message}"
  | .malformedStatus output => s!"HTTP response had no usable status: {output}"
  | .bodyTooLarge actual limit => s!"HTTP response exceeded {limit} bytes ({actual} captured)"

private def responseErrorMessage : SystemOnTPTP.ResponseError → String
  | .httpStatus status => s!"SystemOnTPTP returned HTTP {status}"
  | .missingStatus => "SystemOnTPTP response did not contain an SZS status"
  | .unsupportedStatus status => s!"SystemOnTPTP returned unsupported SZS status `{status}`"

private def runOnline (options : OnlineOptions) : IO UInt32 := do
  try
    let problem ← readProblem options.problem
    let config : SystemOnTPTP.Config := {
      systemLabel := options.system
      endpoint := options.endpoint.getD "https://tptp.org/cgi-bin/SystemOnTPTP"
      timeLimit := options.timeout.getD 30
      maxBodyBytes := options.maxOutput.getD (4 * 1024 * 1024)
    }
    let response ← withProgress s!"online {options.system}" <| SystemOnTPTP.submit config problem
    match response with
    | .error error => printDiagnostic (httpErrorMessage error)
    | .ok response =>
        match SystemOnTPTP.parseResponse config problem response with
        | .error error => printDiagnostic (responseErrorMessage error)
        | .ok artifact => showArtifact artifact
  catch error => printDiagnostic s!"could not read problem: {error}"

def main (argv : List String) : IO UInt32 :=
  Argus.Term.main cli argv fun action =>
    match action with
    | .local options => runLocal options
    | .online options => runOnline options
    | .doctor => runDoctor
