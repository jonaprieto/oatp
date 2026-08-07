/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Argus
import Argus.Term
import OATP
import Std.Async.Process
import TermColor.Diagnostics
import TermColor.Terminal

open Argus
open OATP
open TermColor
open TermColor.Diagnostics
open TermColor.Terminal
open scoped TermColor.Style

structure CliIdentity where
  name : String
  version : String
  endpoint : String

private def cliIdentity : IO CliIdentity := do
  let executable ← Std.IO.Process.getExecutablePath
  let executableName := executable.fileName.getD "tool"
  pure {
    name := (← IO.getEnv "OATP_NAME").getD executableName
    version := (← IO.getEnv "OATP_VERSION").getD "development"
    endpoint := (← IO.getEnv "OATP_SYSTEM_ENDPOINT").getD SystemOnTPTP.defaultEndpoint
  }

argus_opts LocalOptions where
  executable : String := Spec.flag "executable" (some 'x') "Local prover executable" Param.path;
  problem : String := Spec.arg "PROBLEM" "TPTP problem file" Param.path;
  timeout : Option Nat := Spec.opt (Spec.flag "timeout" (some 't')
    "Wall-clock limit" Param.duration);
  maxOutput : Option Nat := Spec.opt (Spec.flag "max-output" none
    "Maximum captured output" Param.bytes);
  arguments : List String := Spec.many
    (Spec.arg "ARG" "Argument passed to the prover after --" Param.str)

argus_opts RunOptions where
  problem : String := Spec.arg "PROBLEM" "TPTP problem file" Param.path;
  provers : List String := Spec.many (Spec.flag "prover" (some 'p')
    "Prover reference; repeat for a portfolio (online-* opts into network use)" Param.str);
  endpoint : Option String := Spec.opt (Spec.flag "endpoint" none
    "SystemOnTPTP endpoint for online-* provers" Param.str);
  refresh : Bool := Spec.switch "refresh" none "Refresh the online prover catalogue";
  noCache : Bool := Spec.switch "no-cache" none "Do not read or write the online catalogue cache";
  timeout : Option Nat := Spec.opt (Spec.flag "timeout" (some 't')
    "Wall-clock limit" Param.duration);
  maxOutput : Option Nat := Spec.opt (Spec.flag "max-output" none
    "Maximum captured output" Param.bytes);
  arguments : List String := Spec.many
    (Spec.arg "ARG" "Argument passed to the prover after --" Param.str)

argus_opts OnlineOptions where
  system : String := Spec.flag "system" (some 's') "SystemOnTPTP system label" Param.str;
  problem : String := Spec.arg "PROBLEM" "TPTP problem file" Param.path;
  endpoint : Option String := Spec.opt (Spec.flag "endpoint" none
    "SystemOnTPTP endpoint" Param.str);
  timeout : Option Nat := Spec.opt (Spec.flag "timeout" (some 't')
    "Remote time limit" Param.duration);
  maxOutput : Option Nat := Spec.opt (Spec.flag "max-output" none
    "Maximum captured response" Param.bytes)

argus_opts SystemsOptions where
  online : Bool := Spec.switch "online" (some 'o') "Also fetch online provers";
  endpoint : Option String := Spec.opt (Spec.flag "endpoint" none
    "SystemOnTPTP endpoint" Param.str);
  refresh : Bool := Spec.switch "refresh" none "Refresh the online prover catalogue";
  noCache : Bool := Spec.switch "no-cache" none "Do not read or write the online catalogue cache"

inductive Action where
  | run (options : RunOptions)
  | local (options : LocalOptions)
  | online (options : OnlineOptions)
  | systems (options : SystemsOptions)
  | doctor

def cli (identity : CliIdentity) : Command Action :=
  Argus.group identity.name
    [ Argus.cmd "run" (Spec.map Action.run RunOptions.spec)
        (description := "Run a local or explicitly selected online portfolio")
    , Argus.cmd "local" (Spec.map Action.local LocalOptions.spec)
        (description := "Run a local ATP process")
    , Argus.cmd "online" (Spec.map Action.online OnlineOptions.spec)
        (description := "Submit a problem to SystemOnTPTP")
    , Argus.cmd "systems" (Spec.map Action.systems SystemsOptions.spec)
        (description := "List installed local and available online provers")
    , Argus.cmd "doctor" (Spec.const Action.doctor)
        (description := "Check local tools and online prover readiness") ]
    (version := some identity.version)
    (description := "Run TPTP problems with local and explicitly selected online provers")

private def localProverCandidates : IO (Array String) := do
  match ← IO.getEnv "OATP_LOCAL_PROVERS" with
  | some value =>
      pure <| value.splitOn "," |>.map (·.trimAscii.toString) |>.filter (!·.isEmpty) |>.toArray
  | none => pure #["eprover", "vampire", "metis"]

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

private def doctorOnlineProblem : Problem := {
  name := "oatp-doctor"
  source := "fof(oatp_doctor, conjecture, (p => p)).\n"
}

private def doctorOnlineAttempts (endpoint : String)
    (systems : Array SystemOnTPTP.Catalogue.SystemInfo) : Array Portfolio.Attempt :=
  systems.map fun system => {
    name := system.id
    limits := { wallSeconds := 5, maxOutputBytes := 1024 * 1024 }
    backend := .online {
      endpoint
      systemLabel := system.id
      systemCommands := if system.command.isEmpty then #[] else #[(system.id, system.command)]
      timeLimit := 5
      maxBodyBytes := 1024 * 1024
    }
  }

private def doctorOnlineResult : Portfolio.Result → IO Bool
  | .artifact attempt artifact => do
      doctorRow attempt.name s!"responded: {artifact.status} ({artifact.elapsedMs}ms)" true
      pure true
  | .failed attempt message => do
      let responded := message.startsWith "SystemOnTPTP returned unsupported SZS status"
      let message := message.splitOn "\n" |>.headD "failed"
      doctorRow attempt.name (if responded then "responded: " ++ message else message) responded
      pure responded

private def printDiagnostic (message : String) : IO UInt32 := do
  let stderr ← IO.getStderr
  let target ← TermColor.targetWithTty .auto (← stderr.isTty)
  let diagnostic := TermColor.Diagnostics.render #[] (Diagnostic.error message)
  stderr.putStr (Text.render target diagnostic)
  stderr.putStr "\n"
  pure 1

private partial def progressLoop (finished : IO.Ref Bool)
    (config : Widgets.ProgressConfig) (state : Widgets.IndeterminateProgressState)
    (region : LiveRegion) : IO Unit := do
  if ← finished.get then
    let _ ← region.finish
    pure ()
  else
    let state := { state with frame := state.frame + 1 }
    let region ← region.updateText (Widgets.indeterminateProgressBar config state)
    IO.sleep 120
    progressLoop finished config state region

private def withProgress {α : Type} (label : String) (action : IO α) : IO α := do
  if !(← stdoutSupportsControl) then
    return ← action
  withHiddenCursor do
    let finished ← IO.mkRef false
    let config : Widgets.ProgressConfig := {
      width := 24
      indeterminateWidth := 7
    }
    let progress ← IO.asTask (do
      progressLoop finished config { label := Text.plain label } LiveRegion.start
      ) Task.Priority.dedicated
    let result ← try action finally
      finished.set true
      let _ ← IO.ofExcept progress.get
    pure result

private def httpErrorMessage : OATP.Http.Error → String
  | .io message => s!"HTTP IO failed: {message}"
  | .invalidRequest message => s!"invalid HTTP request: {message}"
  | .transport message => s!"HTTP transport failed: {message}"
  | .malformedStatus output => s!"HTTP response had no usable status: {output}"
  | .requestBodyTooLarge actual limit =>
      s!"HTTP request exceeded {limit} bytes ({actual} captured)"
  | .bodyTooLarge actual limit => s!"HTTP response exceeded {limit} bytes ({actual} captured)"

private def readProblem (path : String) : IO Problem := do
  pure { name := path, source := ← IO.FS.readFile path }

inductive CatalogueCache where
  | normal
  | refresh
  | noCache

private def catalogueLocation (cacheNamespace endpoint : String) :
    IO (Option (System.FilePath × System.FilePath)) := do
  let root ← match ← IO.getEnv "XDG_CACHE_HOME" with
    | some path => pure (some (⟨path⟩ : System.FilePath))
    | none => match ← IO.getEnv "HOME" with
      | some path => pure (some (System.FilePath.join (⟨path⟩ : System.FilePath) ".cache"))
      | none => pure none
  let safeNamespace := String.ofList (cacheNamespace.toList.map fun character =>
    if character.isAlphanum || character == '-' || character == '_' then character else '_')
  pure <| root.map fun root =>
    let cacheName := if safeNamespace.isEmpty then "tool" else safeNamespace
    let normalizedEndpoint := endpoint.trimAscii.toString
    let directory := System.FilePath.join root cacheName
    (directory, System.FilePath.join directory s!"systems-{hash normalizedEndpoint}.html")

private def fetchCatalogue (endpoint : String)
    (location : Option (System.FilePath × System.FilePath))
    (writeCache : Bool) : IO (Except String (Array SystemOnTPTP.Catalogue.SystemInfo)) := do
  match ← SystemOnTPTP.fetchCatalogue endpoint with
  | .error error => pure (.error (httpErrorMessage error))
  | .ok response =>
      if response.statusCode < 200 || response.statusCode ≥ 300 then
        pure (.error s!"SystemOnTPTP catalogue returned HTTP {response.statusCode}")
      else
        let systems := SystemOnTPTP.Catalogue.parse response.body
        if systems.isEmpty then
          pure (.error "SystemOnTPTP catalogue contained no prover systems")
        else
          if writeCache then
            for (directory, path) in location do
              try
                IO.FS.createDirAll directory
                IO.FS.writeFile path response.body
              catch _ => pure ()
          pure (.ok systems)

private def loadCatalogue (cacheNamespace endpoint : String) (mode : CatalogueCache) :
    IO (Except String (Array SystemOnTPTP.Catalogue.SystemInfo)) := do
  let location ← catalogueLocation cacheNamespace endpoint
  match mode with
  | .normal =>
      match location with
      | some (_, path) =>
          try
            let systems := SystemOnTPTP.Catalogue.parse (← IO.FS.readFile path)
            if systems.isEmpty then
              fetchCatalogue endpoint location true
            else pure (.ok systems)
          catch _ => fetchCatalogue endpoint location true
      | none => fetchCatalogue endpoint none false
  | .refresh => fetchCatalogue endpoint location true
  | .noCache => fetchCatalogue endpoint none false

private def showArtifact (artifact : Artifact) : IO UInt32 := do
  let marker := if artifact.status == .theorem then "✓" else "!"
  IO.eprintln s!"{marker} {artifact.prover.label}: {artifact.status} ({artifact.elapsedMs}ms)"
  unless artifact.stdout.isEmpty || artifact.stdout.trimAscii.toString.startsWith "<!DOCTYPE" do
    IO.print artifact.stdout
  unless artifact.stderr.isEmpty do IO.eprint artifact.stderr
  pure <| if artifact.status == .theorem then 0 else 1

private def processErrorMessage (toolName executable : String) : OATP.Process.Error → String
  | .io message =>
      s!"could not start local prover `{executable}`: {message}; " ++
        s!"check the path or run `{toolName} doctor`"
  | .outputTooLarge actual limit =>
      s!"local prover output exceeded {limit} bytes ({actual} captured)"

private def runLocal (toolName : String) (options : LocalOptions) : IO UInt32 := do
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
    | .error error => printDiagnostic (processErrorMessage toolName options.executable error)
  catch error => printDiagnostic s!"could not read problem: {error}"

private def installedProvers : IO (Array String) := do
  let mut found := #[]
  for executable in ← localProverCandidates do
    if (← Http.commandVersion executable).isSome then
      found := found.push executable
  pure found

private def noLocalProverMessage : IO String := do
  let candidates ← localProverCandidates
  pure <| s!"no local ATP found; tried {String.intercalate ", " candidates.toList}. " ++
    "Install one, pass --prover PATH, or explicitly select an online-* prover"

private def responseErrorMessage : SystemOnTPTP.ResponseError → String
  | .httpStatus status => s!"SystemOnTPTP returned HTTP {status}"
  | .missingStatus => "SystemOnTPTP response did not contain an SZS status"
  | .unsupportedStatus status => s!"SystemOnTPTP returned unsupported SZS status `{status}`"

private def showPortfolioResult : Portfolio.Result → IO Bool
  | .artifact _ artifact => do
      let marker := if artifact.status == .theorem then "✓" else "!"
      IO.eprintln s!"{marker} {artifact.prover.label}: {artifact.status} ({artifact.elapsedMs}ms)"
      unless artifact.stdout.isEmpty || artifact.stdout.trimAscii.toString.startsWith "<!DOCTYPE" do
        IO.print artifact.stdout
      unless artifact.stderr.isEmpty do IO.eprint artifact.stderr
      pure (artifact.status == .theorem)
  | .failed attempt message => do
      IO.eprintln s!"! {attempt.name}: {message}"
      pure false

private def portfolioStatus : Portfolio.Result → Text
  | .artifact _ artifact =>
      let style := match artifact.status with
        | .theorem => Style.fg doctorPalette.green
        | .timeout => Style.fg doctorPalette.yellow
        | .error => Style.fg doctorPalette.red
        | _ => Style.fg doctorPalette.foreground
      Text.styled (toString artifact.status) style
  | .failed _ _ => Text.styled "failed" (Style.fg doctorPalette.red)

private def portfolioElapsed : Portfolio.Result → String
  | .artifact _ artifact => s!"{artifact.elapsedMs}ms"
  | .failed _ _ => "-"

private def portfolioName : Portfolio.Result → String
  | .artifact attempt _ => attempt.name
  | .failed attempt _ => attempt.name

private def portfolioRows (results : List Portfolio.Result) : List (List Text) :=
  [ [ Text.styled "prover" (Style.bold <+> Style.fg doctorPalette.purple)
    , Text.styled "status" (Style.bold <+> Style.fg doctorPalette.purple)
    , Text.styled "time" (Style.bold <+> Style.fg doctorPalette.purple) ] ] ++
  results.map fun result =>
    [ Text.plain (portfolioName result)
    , portfolioStatus result
    , Text.plain (portfolioElapsed result) ]

private def portfolioView (total : Nat) (progress : Widgets.IndeterminateProgressState)
    (results : List Portfolio.Result) : Text :=
  let label := s!"running {total} prover(s) · {results.length}/{total} done"
  let progress := Widgets.indeterminateProgressBar {
    width := 28
    indeterminateWidth := 8
    filledStyle := Style.fg doctorPalette.cyan
    emptyStyle := Style.dim <+> Style.fg doctorPalette.comment
  } { progress with label := Text.styled label (Style.fg doctorPalette.foreground) }
  progress ++ Text.plain "\n" ++ Widgets.renderTable [28, 16, 12] (portfolioRows results)

private partial def portfolioProgressLoop (finished : IO.Ref Bool)
    (progress : IO.Ref Widgets.IndeterminateProgressState)
    (results : IO.Ref (List Portfolio.Result)) (region : IO.Ref LiveRegion)
    (total : Nat) : IO Unit := do
  if ← finished.get then
    pure ()
  else
    progress.modify fun state => { state with frame := state.frame + 1 }
    let next ← do
      let live ← region.get
      let currentProgress ← progress.get
      let currentResults ← results.get
      live.updateText (portfolioView total currentProgress currentResults)
    region.set next
    IO.sleep 120
    portfolioProgressLoop finished progress results region total

private def withPortfolioProgress {α : Type} (total : Nat)
    (action : (Portfolio.Result → IO Unit) → IO α) : IO α := do
  if !(← stdoutSupportsControl) then
    return ← action (fun _ => pure ())
  withHiddenCursor do
    let finished ← IO.mkRef false
    let progress ← IO.mkRef ({ : Widgets.IndeterminateProgressState })
    let results ← IO.mkRef ([] : List Portfolio.Result)
    let region ← IO.mkRef LiveRegion.start
    let tick ← IO.asTask (portfolioProgressLoop finished progress results region total)
      Task.Priority.dedicated
    let onResult := fun result => do
      results.modify (· ++ [result])
      let next ← do
        let live ← region.get
        let currentProgress ← progress.get
        let currentResults ← results.get
        live.updateText (portfolioView total currentProgress currentResults)
      region.set next
    let value ← try
      action onResult
    finally
      finished.set true
      let _ ← IO.ofExcept tick.get
      let next ← do
        let live ← region.get
        let currentProgress ← progress.get
        let currentResults ← results.get
        live.updateText (portfolioView total currentProgress currentResults)
      let _ ← next.finish
    pure value

private def runDoctorOnline (identity : CliIdentity) (transports : Array String) : IO Bool := do
  doctorSection "ONLINE ATP (probe)"
  if transports.isEmpty then
    doctorRow "service" "unavailable (no HTTP transport)" false
    pure false
  else
    match ← loadCatalogue identity.name identity.endpoint .refresh with
    | .error message =>
        doctorRow "service" message false
        pure false
    | .ok systems =>
        doctorRow "service" s!"available ({systems.size} systems)" true
        let results ← withPortfolioProgress systems.size fun onResult =>
          Portfolio.runWith doctorOnlineProblem
            (doctorOnlineAttempts identity.endpoint systems) onResult
        let mut working := false
        for result in results do
          working := (← doctorOnlineResult result) || working
        pure working

private def runDoctor (identity : CliIdentity) : IO UInt32 := do
  let transports ← Http.availableTransports
  let (online, ready) :=
    if transports.contains "curl" then
      ("ready (curl preferred)", true)
    else if transports.contains "wget" then
      ("ready (wget fallback)", true)
    else
      ("unavailable (install curl or wget)", false)
  writeTextLine (Text.styled s!"{identity.name} doctor {identity.version}"
    (Style.bold <+> Style.fg doctorPalette.purple))
  doctorSection "SYSTEM"
  doctorRow "platform" (← doctorPlatform) true

  doctorSection "TRANSPORT"
  for command in #["curl", "wget"] do
    doctorTool command
  doctorRow "online" online ready

  let onlineWorking ← runDoctorOnline identity transports

  doctorSection "LOCAL ATP"
  for command in ← localProverCandidates do
    doctorTool command

  doctorSection "OPTIONAL"
  doctorTool "docker"
  pure <| if transports.isEmpty || !onlineWorking then 1 else 0

private def runPortfolio (problem : Problem) (attempts : Array Portfolio.Attempt) : IO UInt32 := do
  let results ← withPortfolioProgress attempts.size fun onResult =>
    Portfolio.runWith problem attempts onResult
  let mut success := false
  for result in results do
    success := (← showPortfolioResult result) || success
  pure <| if success then 0 else 1

private def resolveOnline (toolName : String) (systems : Array SystemOnTPTP.Catalogue.SystemInfo)
    (references : List String) : Except String (Array SystemOnTPTP.Catalogue.SystemInfo) := do
  let resolved ← references.mapM fun reference =>
    match SystemOnTPTP.Catalogue.resolve systems reference with
    | some system => pure system
    | none => Except.error (s!"online prover `{reference}` is not in the catalogue; " ++
          s!"run `{toolName} systems --online --refresh`")
  pure resolved.toArray

private def runDefault (identity : CliIdentity) (options : RunOptions) : IO UInt32 := do
  try
    let problem ← readProblem options.problem
    let references ← if options.provers.isEmpty then
      pure (← installedProvers).toList
    else pure options.provers
    let localReferences := references.filter (!·.startsWith "online-")
    let onlineReferences := references.filter (·.startsWith "online-")
    let limits : Limits := {
      wallSeconds := options.timeout.getD 30
      maxOutputBytes := options.maxOutput.getD (4 * 1024 * 1024)
    }
    let mut attempts : Array Portfolio.Attempt := #[]
    for reference in localReferences do
      attempts := attempts.push {
        name := reference
        limits
        backend := .local {
          executable := reference
          arguments := options.arguments.toArray
        }
      }
    if !onlineReferences.isEmpty then
      let endpoint := options.endpoint.getD identity.endpoint
      let mode := if options.noCache then CatalogueCache.noCache
        else if options.refresh then CatalogueCache.refresh else CatalogueCache.normal
      match ← loadCatalogue identity.name endpoint mode with
      | .error message => printDiagnostic message
      | .ok systems =>
          match resolveOnline identity.name systems onlineReferences with
          | .error message => printDiagnostic message
          | .ok resolved =>
              let labels := resolved.map (·.id)
              let commands := resolved.toList.filterMap fun system =>
                if system.command.isEmpty then none else some (system.id, system.command)
              attempts := attempts.push {
                name := "online " ++ String.intercalate ", " labels.toList
                limits
                backend := .online {
                  endpoint
                  systemLabel := labels.getD 0 ""
                  systemLabels := labels
                  systemCommands := commands.toArray
                  timeLimit := limits.wallSeconds
                  maxBodyBytes := limits.maxOutputBytes
                }
              }
              if attempts.isEmpty then
                printDiagnostic (← noLocalProverMessage)
              else runPortfolio problem attempts
    else if attempts.isEmpty then
      printDiagnostic (← noLocalProverMessage)
    else runPortfolio problem attempts
  catch error => printDiagnostic s!"could not read problem: {error}"

private def runSystems (identity : CliIdentity) (options : SystemsOptions) : IO UInt32 := do
  writeTextLine (Text.styled s!"{identity.name} systems {identity.version}"
    (Style.bold <+> Style.fg doctorPalette.purple))
  writeTextLine (Text.styled "LOCAL" (Style.bold <+> Style.fg doctorPalette.cyan))
  let localProvers ← installedProvers
  if localProvers.isEmpty then
    let candidates ← localProverCandidates
    writeTextLine (Text.plain (s!"  (none found — candidates: " ++
      String.intercalate ", " candidates.toList ++ ")"))
  else
    for executable in localProvers do
      let version := (← Http.commandVersion executable).getD "installed"
      writeTextLine (Text.plain s!"  {executable}  {version}")
  if options.online then
    let endpoint := options.endpoint.getD identity.endpoint
    let mode := if options.noCache then CatalogueCache.noCache
      else if options.refresh then CatalogueCache.refresh else CatalogueCache.normal
    writeTextLine Text.empty
    writeTextLine (Text.styled s!"ONLINE  {endpoint}"
      (Style.bold <+> Style.fg doctorPalette.cyan))
    match ← loadCatalogue identity.name endpoint mode with
    | .error message => printDiagnostic message
    | .ok systems =>
        for system in systems do
          let name := SystemOnTPTP.Catalogue.baseName system.id
          writeTextLine (Text.plain s!"  online-{system.id}  ({name})")
        pure 0
  else
    writeTextLine Text.empty
    writeTextLine (Text.styled
      s!"Online systems are opt-in: use `{identity.name} systems --online`."
      (Style.fg doctorPalette.comment))
    pure 0

private def submitOnline (options : OnlineOptions) (problem : Problem) (endpoint : String)
    (system : SystemOnTPTP.Catalogue.SystemInfo) : IO UInt32 := do
  let config : SystemOnTPTP.Config := {
    systemLabel := system.id
    systemCommands := if system.command.isEmpty then #[] else #[(system.id, system.command)]
    endpoint
    timeLimit := options.timeout.getD 30
    maxBodyBytes := options.maxOutput.getD (4 * 1024 * 1024)
  }
  let started ← IO.monoMsNow
  let response ← withProgress s!"online {system.id}" <| SystemOnTPTP.submit config problem
  match response with
  | .error error => printDiagnostic (httpErrorMessage error)
  | .ok response =>
      match SystemOnTPTP.parseResponse config problem response with
      | .error error => printDiagnostic (responseErrorMessage error)
      | .ok artifact => showArtifact { artifact with elapsedMs := (← IO.monoMsNow) - started }

private def runOnline (identity : CliIdentity) (options : OnlineOptions) : IO UInt32 := do
  try
    let problem ← readProblem options.problem
    let endpoint := options.endpoint.getD identity.endpoint
    match ← loadCatalogue identity.name endpoint CatalogueCache.normal with
    | .error message => printDiagnostic message
    | .ok systems =>
        let system? := SystemOnTPTP.Catalogue.resolve systems options.system
        if system?.isSome then
          submitOnline options problem endpoint (system?.getD { id := options.system })
        else
          printDiagnostic (s!"online system `{options.system}` is not in the catalogue; " ++
            s!"run `{identity.name} systems --online`")
  catch error => printDiagnostic s!"could not read problem: {error}"

def main (argv : List String) : IO UInt32 := do
  let identity ← cliIdentity
  let command := cli identity
  if argv.isEmpty then
    let _ ← Argus.Term.printHelp command
    pure 0
  else
    Argus.Term.main command argv fun action =>
      match action with
      | .run options => runDefault identity options
      | .local options => runLocal identity.name options
      | .online options => runOnline identity options
      | .systems options => runSystems identity options
      | .doctor => runDoctor identity
