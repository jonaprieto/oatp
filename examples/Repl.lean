/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP
import OATP.ReplView
import TermColor.Diagnostics
import TermColor.Repl.Terminal
import TermColor.Terminal

/-!
# oatp repl

The interactive shell combines the pure TPTP session model with OATP's existing process, portfolio,
catalogue, and Lean proof boundaries. Static script mode keeps the same command path testable in CI.
-/

open OATP
open OATP.ReplView
open Lean
open TermColor
open TermColor.Diagnostics
open TermColor.Repl
open TermColor.Terminal
open TermColor.Widgets

private def words (line : String) : List String :=
  line.splitOn " " |>.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def appendEntry (app : App) (cell : Nat) (input output : String) (ok : Bool)
    (elapsedMs : Option Nat := none) (sources : Sources := #[])
    (diagnostic : Option Diagnostic := none) : App :=
  { app with
    entries := { cell, input, output, ok, elapsedMs, sources, diagnostic } :: app.entries
    repl := {} 
    status := if ok then "ready" else "error" }

private def diagnosticFor (source message : String) : Source × Diagnostic :=
  let sourceText := source
  let source := Source.fromBytes "input" source.toUTF8
  let title := message.splitOn "\n" |>.headD message
  let diagnostic := (Diagnostic.error title).withLabel
    (Label.primary (Span.range 0 0 sourceText.toUTF8.size))
  (source, diagnostic)

private def note (app : App) (cell : Nat) (input output : String) (ok : Bool)
    (elapsedMs : Option Nat := none) : App :=
  let (sources, diagnostic) := if ok then
      (#[], none)
    else
      let (source, diagnostic) := diagnosticFor input output
      (#[(source)], some diagnostic)
  appendEntry { app with session := OATP.Repl.note app.session input output } cell input output ok
    elapsedMs sources diagnostic

private def noteDiagnostic (app : App) (cell : Nat) (input source message : String) : App :=
  let (source, diagnostic) := diagnosticFor source message
  appendEntry { app with session := OATP.Repl.note app.session input message } cell input message false
    none #[source] (some diagnostic)

private def clearDerived (app : App) : App :=
  { app with goal := none, translation := none, term := none, leanGoal := none }

private def changesContext (input : String) : Bool :=
  match OATP.Repl.parseInput input with
  | .source _ => true
  | .command command => match command with
      | .parse _ | .axiom _ _ | .conjecture _ _ | .clear | .reset => true
      | _ => false

private def lastHistory (session : OATP.Repl.Session) : String :=
  session.history.toList.reverse.head?.map (·.result) |>.getD "ok"

private def artifactText : Portfolio.Result → (String × Bool)
  | .artifact attempt artifact =>
      let head := s!"{attempt.name}: {artifact.status} ({artifact.elapsedMs}ms)"
      let stdout := if artifact.stdout.isEmpty then "" else "\n" ++ artifact.stdout.trimAscii.toString
      let stderr := if artifact.stderr.isEmpty then "" else "\nstderr: " ++ artifact.stderr.trimAscii.toString
      (head ++ stdout ++ stderr, artifact.status == .theorem)
  | .failed attempt message => (s!"{attempt.name}: failed: {message}", false)

private def currentProblem (app : App) : Option Problem :=
  app.translation.map (fun source => { name := "lean-goal", source }) |>.orElse
    (fun _ => OATP.Repl.problem app.session)

private def preferences (app : App) : OATP.Config.Preferences := {
  theory := app.theory
  defaultProver := app.defaultProver
  enabledProvers := app.enabledProvers
  proverSelectionSet := app.proverSelectionSet
  theme := app.themeName
}

private def savePreferences (app : App) : IO App := do
  match ← OATP.Config.save (preferences app) with
  | none => pure app
  | some message => pure { app with statusNotice := some message }

private def selectedProvers (app : App) (all : Bool) : IO (Array String) := do
  let installed ← OATP.Runtime.installedProvers
  if all then pure installed
  else if app.proverSelectionSet then
    pure <| app.enabledProvers.filter (fun name => installed.any (· == name))
  else if !app.defaultProver.isEmpty then
    pure <| if installed.any (· == app.defaultProver) then #[app.defaultProver] else #[]
  else pure installed

private def runRequest (app : App) (request : OATP.Repl.RunRequest) : IO (String × Bool) := do
  match currentProblem app with
  | none => pure ("no current problem; add a TPTP conjecture or translate a Lean goal", false)
  | some problem =>
      let references ← if request.references.isEmpty then
        pure (← selectedProvers app request.all).toList
      else pure request.references
      let localReferences := references.filter (!·.startsWith "online-")
      let onlineReferences := references.filter (·.startsWith "online-")
      let limits : Limits := { wallSeconds := request.timeout, maxOutputBytes := request.maxOutput }
      let mut attempts : Array Portfolio.Attempt := #[]
      for reference in localReferences do
        attempts := attempts.push {
          name := reference
          limits
          backend := .local { executable := reference, arguments := request.arguments.toArray }
        }
      if !onlineReferences.isEmpty then
        let endpoint := request.endpoint.getD SystemOnTPTP.defaultEndpoint
        let mode := if request.noCache then OATP.Runtime.CatalogueCache.noCache
          else if request.refresh then OATP.Runtime.CatalogueCache.refresh
          else OATP.Runtime.CatalogueCache.normal
        match ← OATP.Runtime.loadCatalogue "oatp-repl" endpoint mode with
        | .error message => return (message, false)
        | .ok systems =>
            match OATP.Runtime.resolveOnline "oatp-repl" systems onlineReferences with
            | .error message => return (message, false)
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
                    timeLimit := request.timeout
                    maxBodyBytes := request.maxOutput
                  }
                }
      if attempts.isEmpty then
        pure ("no prover selected or installed", false)
      else
        let results ← Portfolio.run problem attempts
        let rendered := results.toList.map artifactText
        pure (String.intercalate "\n" (rendered.map Prod.fst), rendered.any Prod.snd)

private def backendLine (input : String) : Bool :=
  input == "/run" || input.startsWith "/run " || input == "/local" || input.startsWith "/local " ||
  input == "/online" || input.startsWith "/online "

private def backendRequest (input : String) : Except String OATP.Repl.RunRequest :=
  let args := words input |>.drop 1
  if input == "/run" || input.startsWith "/run " then
    OATP.Repl.parseRunRequest args
  else if input == "/local" || input.startsWith "/local " then
    match OATP.Repl.parseLocalRequest args with
    | .error message => .error message
    | .ok request => pure {
        references := [request.executable]
        timeout := request.timeout
        maxOutput := request.maxOutput
        arguments := request.arguments }
  else
    match OATP.Repl.parseOnlineRequest args with
    | .error message => .error message
    | .ok request => pure {
        references := [if request.system.startsWith "online-" then request.system
          else "online-" ++ request.system]
        endpoint := request.endpoint
        timeout := request.timeout
        maxOutput := request.maxOutput }

private def backgroundJobs : TermColor.Repl.Terminal.JobConfig App where
  shouldRun := fun _ input => backendLine input
  start := fun app _ => { app with busy := true, jobResult := none, repl := {} }
  run := fun cancellation app input => do
    let started ← IO.monoMsNow
    unless ← TermColor.Repl.Terminal.Cancellation.sleep cancellation 1 do
      return app
    match backendRequest input with
    | .error message =>
        pure { app with jobResult := some {
          cell := app.session.nextCell, input, output := message, ok := false,
          elapsedMs := some ((← IO.monoMsNow) - started) } }
    | .ok request =>
        -- partiality: portfolio execution has no process-cancellation seam yet; keep the UI
        -- responsive and add cancellation at Portfolio once process ownership is exposed.
        let (output, ok) ← runRequest app request
        pure { app with jobResult := some {
          cell := app.session.nextCell, input, output, ok,
          elapsedMs := some ((← IO.monoMsNow) - started) } }
  finish := fun current completed =>
    match completed.jobResult with
    | some result =>
        note { current with busy := false, jobResult := none }
          result.cell result.input result.output result.ok result.elapsedMs
    | none => { current with busy := false }
  cancel := fun app => { app with busy := false, jobResult := none }
  fail := fun app message =>
    note { app with busy := false, jobResult := none } app.session.nextCell
      "background prover" message false

private def systemsText (request : OATP.Repl.SystemsRequest) : IO String := do
  let installed ← OATP.Runtime.installedProvers
  let lines := if installed.isEmpty then ["LOCAL: none"] else
      ["LOCAL:"] ++ installed.toList.map (fun prover => "  " ++ prover)
  if !request.online then
    pure <| String.intercalate "\n" lines ++ "\nONLINE: opt-in with /systems --online"
  else
    let endpoint := request.endpoint.getD SystemOnTPTP.defaultEndpoint
    let mode := if request.noCache then OATP.Runtime.CatalogueCache.noCache
      else if request.refresh then OATP.Runtime.CatalogueCache.refresh
      else OATP.Runtime.CatalogueCache.normal
    match ← OATP.Runtime.loadCatalogue "oatp-repl" endpoint mode with
    | .error message => pure <| String.intercalate "\n" lines ++ "\nONLINE: " ++ message
    | .ok systems =>
        pure <| String.intercalate "\n" (lines ++ ["ONLINE:"] ++
          systems.toList.map (fun system => "  online-" ++ system.id))

private def doctorText : IO String := do
  let transports ← Http.availableTransports
  let installed ← OATP.Runtime.installedProvers
  let platform ← try
      let output ← IO.Process.output { cmd := "uname", args := #["-s", "-m"] }
      pure <| if output.exitCode == 0 then output.stdout.trimAscii.toString else "unavailable"
    catch _ => pure "unavailable"
  let transportText := if transports.isEmpty then "unavailable" else
    String.intercalate ", " transports.toList
  let localText := if installed.isEmpty then "none" else
    String.intercalate ", " installed.toList
  pure <| String.intercalate "\n" [
    "OATP REPL doctor",
    "",
    "SYSTEM",
    "  platform:  " ++ platform,
    "  version:   " ++ OATP.version,
    "",
    "TRANSPORT",
    "  available: " ++ transportText,
    "",
    "LOCAL ATP",
    "  installed: " ++ localText,
    "",
    "ONLINE",
    "  status:    " ++ if transports.isEmpty then "unavailable" else "ready (explicit /online)",
    "",
    "Use /systems --online to inspect the remote catalogue."
  ]

private def fuzzy (query : String) (value : String) : Bool :=
  let query := query.toLower
  let value := value.toLower
  value == query || value.startsWith query || value.contains query

private def infoText (app : App) (query : String) : IO (String × Bool) := do
  let candidates ← OATP.Runtime.localProverCandidates
  let found := candidates.filter (fuzzy query)
  if found.size == 1 then
    let name := found[0]!
    let version := (← Http.commandVersion name).getD "not installed"
    let installed := (← OATP.Runtime.installedProvers).any (· == name)
    let enabled := if app.proverSelectionSet then app.enabledProvers.any (· == name) else installed
    return (String.intercalate "\n" [
      s!"prover: {name}",
      s!"kind:   local executable",
      s!"version: {version}",
      s!"default: {if app.defaultProver == name then "yes" else "no"}",
      s!"enabled: {if enabled then "yes" else "no"}"
    ], true)
  if found.size > 1 then
    return (s!"`{query}` matches local provers: {String.intercalate ", " found.toList}", false)
  let endpoint := SystemOnTPTP.defaultCatalogueEndpoint
  match ← OATP.Runtime.loadCatalogue "oatp-repl" endpoint .normal with
  | .error _ => pure (s!"no prover matched `{query}`; local candidates: {
      String.intercalate ", " candidates.toList}", false)
  | .ok systems =>
      let online := systems.filter (fun system => fuzzy query system.id || fuzzy query (SystemOnTPTP.Catalogue.baseName system.id))
      match online.toList with
      | [] => pure (s!"no prover matched `{query}`", false)
      | [system] => pure (String.intercalate "\n" [
          s!"prover: online-{system.id}",
          "kind:   SystemOnTPTP catalogue",
          s!"command: {if system.command.isEmpty then "catalogue default" else system.command}",
          s!"time limit: {system.timeLimit}s"
        ], true)
      | _ => pure (s!"`{query}` matches online provers: {
          String.intercalate ", " (online.toList.map (fun system => "online-" ++ system.id))}", false)

private partial def parseStepTokens : List String → Except String (OATP.Proof.Step × List String)
  | "true-intro" :: rest => pure (.trueIntro, rest)
  | "exact" :: [] => .error "exact expects a hypothesis name"
  | "exact" :: name :: rest => pure (.exact (Name.mkSimple name), rest)
  | "and-left" :: [] => .error "and-left expects a hypothesis name"
  | "and-left" :: name :: rest => pure (.andLeft (Name.mkSimple name), rest)
  | "and-right" :: [] => .error "and-right expects a hypothesis name"
  | "and-right" :: name :: rest => pure (.andRight (Name.mkSimple name), rest)
  | "and-intro" :: rest => do
      let (left, rest) ← parseStepTokens rest
      let (right, rest) ← parseStepTokens rest
      pure (.andIntro left right, rest)
  | "implication-intro" :: [] => .error "implication-intro expects a hypothesis name"
  | "implication-intro" :: name :: [] =>
      .error s!"implication-intro `{name}` expects a body step"
  | "implication-intro" :: name :: rest => do
      let (body, rest) ← parseStepTokens rest
      pure (.implicationIntro (Name.mkSimple name) body, rest)
  | [] => .error "expected a proof step"
  | token :: _ => .error s!"unknown proof step `{token}`"

private def parseStep (source : String) : Except String OATP.Proof.Step := do
  let (step, rest) ← parseStepTokens (words source)
  match rest with
  | [] => pure step
  | token :: _ => .error s!"unexpected proof step token `{token}`"

private def submitGoal (app : App) (cell : Nat) (input formula : String) : IO App := do
  let runtime ← match app.leanRuntime with
    | some runtime => pure runtime
    | none => OATP.Lean.Repl.create
  match ← OATP.Lean.Repl.goalFromFormula runtime formula with
  | .error message => pure (noteDiagnostic app cell input formula message)
  | .ok (runtime, goal) =>
      let (runtime, snapshot) ← OATP.Lean.Repl.snapshot runtime goal
      let output := "goal created\n" ++ String.intercalate "\n" snapshot.context.toList ++
        "\n⊢ " ++ snapshot.target
      pure <| note { app with
        leanRuntime := some runtime
        leanGoal := some goal
        goal := some snapshot
        translation := none
        term := none } cell input output true

private def submitTheoryFormula (app : App) (cell : Nat) (input role : String) : IO (Option App) := do
  let command := if role == "axiom" then "/axiom " else "/conjecture "
  if !input.startsWith command then return none
  match words (input.drop command.length |>.trimAscii.toString) with
  | name :: formula =>
      let source := s!"{app.theory}({name}, {role}, {String.intercalate " " formula})."
      match OATP.Repl.parseSource app.session input source with
      | .ok session => return some (note { app with session } cell input
          s!"parsed 1 statement using {app.theory}" true)
      | .error message => return some (note app cell input message false)
  | _ => return some (note app cell input s!"{command}NAME FORMULA" false)

private def submitLeanCommand (app : App) (cell : Nat) (input : String) : IO (Option App) := do
  let line := input.trimAscii.toString
  let goalCommand := ["/goal ", "/to-lean ", "/translate-to-lean "].find? (line.startsWith ·)
  if let some command := goalCommand then
    return some (← submitGoal app cell input (line.drop command.length).trimAscii.toString)
  if line == "/snapshot" then
    match app.leanRuntime, app.leanGoal with
    | some runtime, some goal =>
        let (runtime, snapshot) ← OATP.Lean.Repl.snapshot runtime goal
        let output := String.intercalate "\n" snapshot.context.toList ++ "\n⊢ " ++ snapshot.target
        return some <| note { app with leanRuntime := some runtime, goal := some snapshot }
          cell input output true
    | _, _ => return some (note app cell input "no Lean goal" false)
  if line == "/to-tptp" then
    match app.leanRuntime, app.leanGoal with
    | some runtime, some goal =>
        let (runtime, translation) ← OATP.Lean.Repl.translateToTPTP runtime goal
        match translation with
        | .ok value =>
            return some <| note { app with
              leanRuntime := some runtime
              translation := some value.problem.source } cell input value.problem.source true
        | .error message => return some (note app cell input message false)
    | _, _ => return some (note app cell input "no Lean goal" false)
  if line.startsWith "/reconstruct " then
    match app.leanRuntime, app.leanGoal, parseStep (line.drop "/reconstruct ".length).toString with
    | some runtime, some goal, .ok step =>
        let (runtime, result) ← OATP.Lean.Repl.reconstruct runtime goal step
        match result with
        | .ok term =>
            let output := s!"checked\n{term.term}\n: {term.type}"
            return some <| note { app with
              leanRuntime := some runtime, term := some term } cell input output true
        | .error message => return some (note app cell input message false)
    | _, _, .error message => return some (note app cell input message false)
    | _, _, _ => return some (note app cell input "no Lean goal" false)
  if line == "/term" then
    match app.term with
    | some term => return some (note app cell input s!"{term.term}\n: {term.type}" true)
    | none => return some (note app cell input "no rendered term" false)
  pure none

private def submitCore (app : App) (input : String) : IO App := do
  let input := input.trimAscii.toString
  let cell := app.session.nextCell
  if input == "/quit" || input == "/exit" then
    return { app with running := false }
  if input.startsWith "/load " then
    let path := input.drop "/load ".length |>.trimAscii.toString
    if path.isEmpty then
      return note app cell input "/load expects a TPTP file path" false
    try
      let source ← IO.FS.readFile path
      match OATP.Repl.parseSource app.session input source with
      | .ok session =>
          return appendEntry (clearDerived { app with session }) cell input (lastHistory session) true
      | .error message => return note app cell input message false
    catch error =>
      return note app cell input s!"could not read `{path}`: {error}" false
  if let some result ← submitTheoryFormula app cell input "axiom" then
    return result
  if let some result ← submitTheoryFormula app cell input "conjecture" then
    return result
  if let some result ← submitLeanCommand app cell input then
    return result
  if input == "/state" then
    let session := OATP.Repl.note app.session input "state drawer toggled"
    let updated := { app with session, stateOpen := !app.stateOpen, historyOpen := false }
    let updated := { updated with proversOpen := false }
    return appendEntry updated cell input "state drawer toggled" true
  if input.startsWith "/state " then
    let target := input.drop "/state ".length |>.trimAscii.toString
    match openContextTarget { app with historyOpen := false, proversOpen := false } target with
    | some focused =>
        return note focused cell input s!"state: {target}" true
    | none =>
        return note app cell input
          s!"unknown state target `{target}`; try: {String.intercalate ", " contextTargetNames}" false
  if input == "/history" then
    let session := OATP.Repl.note app.session input "history drawer toggled"
    return appendEntry { app with session, historyOpen := !app.historyOpen, stateOpen := false }
      cell input "history drawer toggled" true
  if input == "/version" then
    return note app cell input s!"oatp {OATP.version}" true
  if input == "/theme" then
    return note app cell input s!"theme: {app.themeName}; available: {themeNames}" true
  if input.startsWith "/theme " then
    let name := input.drop "/theme ".length |>.trimAscii.toString
    match themeByName name with
    | some scheme =>
        let updated ← savePreferences { app with theme := scheme, themeName := name }
        return note updated cell input s!"theme changed to {name}" true
    | none => return note app cell input s!"unknown theme `{name}`; try: {themeNames}" false
  if input == "/theory" then
    return note app cell input s!"theory: {app.theory}; available: fof, cnf, tff" true
  if input.startsWith "/theory " then
    let requested := input.drop "/theory ".length |>.trimAscii.toString.toLower
    let theory := if requested == "tf1" then "tff" else requested
    if theory == "fof" || theory == "cnf" || theory == "tff" then
      let updated ← savePreferences { app with theory }
      return note updated cell input s!"theory set to {theory}" true
    else return note app cell input "unknown theory; use fof, cnf, or tff" false
  if input == "/prover" then
    return note app cell input s!"default prover: {if app.defaultProver.isEmpty then "auto" else app.defaultProver}" true
  if input.startsWith "/prover " then
    let name := input.drop "/prover ".length |>.trimAscii.toString
    let candidates ← OATP.Runtime.localProverCandidates
    let found := candidates.filter (fuzzy name)
    match found.toList with
    | [resolved] =>
        let updated ← savePreferences { app with defaultProver := resolved }
        return note updated cell input s!"default prover set to {resolved}" true
    | [] => return note app cell input s!"unknown prover `{name}`" false
    | _ =>
        let detail := String.intercalate ", " found.toList
        return note app cell input (s!"ambiguous prover `{name}`: " ++ detail) false
  if input == "/provers" then
    let installed ← OATP.Runtime.installedProvers
    let enabled := if !app.proverSelectionSet then installed else
      app.enabledProvers.filter (fun name => installed.any (· == name))
    let output := if installed.isEmpty then "no installed local provers"
      else "use J/K and Space to toggle: " ++ String.intercalate ", " installed.toList
    let updated := { app with proverChoices := installed }
    let updated := { updated with enabledProvers := enabled }
    let updated := { updated with proverSelectionSet := true }
    let updated := { updated with proversOpen := !app.proversOpen }
    let updated := { updated with stateOpen := false }
    let updated := { updated with historyOpen := false }
    return note updated cell input output true
  if input == "/info" then
    return note app cell input "usage: /info PROVER" false
  if input.startsWith "/info " then
    let query := input.drop "/info ".length |>.trimAscii.toString
    let (output, ok) ← infoText app query
    return note app cell input output ok
  if input == "/systems" || input.startsWith "/systems " then
    match OATP.Repl.parseSystemsRequest (words input |>.drop 1) with
    | .error message => return note app cell input message false
    | .ok request =>
        let output ← systemsText request
        return note app cell input output true
  if input == "/doctor" then
    return note app cell input (← doctorText) true
  if input == "/run" || input.startsWith "/run " then
    match OATP.Repl.parseRunRequest (words input |>.drop 1) with
    | .error message => return note app cell input message false
    | .ok request =>
        let (output, ok) ← runRequest app request
        return note app cell input output ok
  if input == "/local" || input.startsWith "/local " then
    match OATP.Repl.parseLocalRequest (words input |>.drop 1) with
    | .error message => return note app cell input message false
    | .ok request =>
        let request : OATP.Repl.RunRequest := {
          references := [request.executable]
          timeout := request.timeout
          maxOutput := request.maxOutput
          arguments := request.arguments
        }
        let (output, ok) ← runRequest app request
        return note app cell input output ok
  if input == "/online" || input.startsWith "/online " then
    match OATP.Repl.parseOnlineRequest (words input |>.drop 1) with
    | .error message => return note app cell input message false
    | .ok request =>
        let reference := if request.system.startsWith "online-" then request.system
          else "online-" ++ request.system
        let request : OATP.Repl.RunRequest := {
          references := [reference]
          endpoint := request.endpoint
          timeout := request.timeout
          maxOutput := request.maxOutput
        }
        let (output, ok) ← runRequest app request
        return note app cell input output ok
  match OATP.Repl.apply app.session input with
  | .ok session =>
      let output := lastHistory session
      let app := if changesContext input then clearDerived { app with session }
        else { app with session }
      let app := if input == "/reset" then { app with entries := [] } else app
      let entryCell := if input == "/reset" then 1 else cell
      pure <| appendEntry app entryCell input output true
  | .error message => pure (note app cell input message false)

private def submit (app : App) (input : String) : IO App := do
  let started ← IO.monoMsNow
  let app ← submitCore app input
  let app ← savePreferences app
  let elapsedMs := (← IO.monoMsNow) - started
  match app.entries with
  | entry :: rest => pure { app with entries := { entry with elapsedMs := some elapsedMs } :: rest }
  | [] => pure app

private def commandNames : List String :=
  ["/help", "/help cnf", "/help fof", "/help tff", "/help lean", "/help run", "/help context",
   "/help grammar", "/history", "/state", "/state formulas", "/state symbols", "/state problem",
   "/state goal", "/state translation", "/state term", "/state all", "/grammar", "/grammar cnf",
   "/grammar fof", "/grammar tff", "/roles", "/roles cnf", "/clear", "/reset", "/load",
   "/axiom", "/conjecture", "/parse",
   "/goal", "/to-lean", "/translate-to-lean", "/snapshot", "/to-tptp", "/reconstruct", "/term",
   "/run", "/local", "/online", "/theory", "/prover", "/provers", "/info", "/theme", "/version",
   "/systems", "/doctor", "/quit", "/exit"]

private def complete (_app : App) (input : TextInputState) : IO (List Completion) := do
  let value := input.value
  if value.startsWith "/state " then
    let fragment := value.drop "/state ".length |>.toString
    pure <| contextTargetNames.filter (·.startsWith fragment) |>.map
      (fun target => { replacement := s!"/state {target}" })
  else if value.startsWith "/theme " then
    let fragment := value.drop "/theme ".length |>.toString
    pure <| themes.map Prod.fst |>.filter (·.startsWith fragment) |>.map
      (fun name => { replacement := s!"/theme {name}" })
  else if value.startsWith "/grammar " then
    let fragment := value.drop "/grammar ".length |>.toString
    if fragment.startsWith "roles " then
      let roleFormat := fragment.drop "roles ".length |>.toString
      pure <| ["cnf", "fof", "tff"].filter (·.startsWith roleFormat) |>.map
        (fun format => { replacement := s!"/grammar roles {format}" })
    else
      pure <| ["cnf", "fof", "tff", "lean", "roles"].filter (·.startsWith fragment) |>.map
        (fun topic => { replacement := s!"/grammar {topic}" })
  else
    pure <| commandNames.filter (·.startsWith value) |>.map (fun replacement => { replacement })

private def handleKey (app : App) (key : Key) : Option App :=
  if app.proversOpen then
    match key with
    | .char 'H' | .escape => some { app with proversOpen := false }
    | .char 'J' | .down => some (focusNextProver app)
    | .char 'K' | .up => some (focusPreviousProver app)
    | .enter | .char ' ' => some (toggleFocusedProver app)
    | _ => none
  else if app.stateOpen then
    match key with
    | .char 'H' => some { app with stateOpen := false }
    | .char 'J' | .down => some (focusNextContext app)
    | .char 'K' | .up => some (focusPreviousContext app)
    | .enter | .char ' ' => some (toggleFocusedContext app)
    | .right => some (expandFocusedContext app)
    | .left | .escape => some (collapseFocusedContext app)
    | _ => none
  else if app.historyOpen then
    match key with
    | .char 'H' => some { app with historyOpen := false }
    | _ => none
  else none

private def handleMouse (app : App) (size : Size) (mouse : MouseEvent) : Option App :=
  if app.proversOpen then
    match drawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let contextLeft := leftWidth + 3
        let contextRight := contextLeft + rightWidth - 1
        if mouse.column < contextLeft || mouse.column > contextRight then none
        else match mouse.action with
        | .scrollUp => some (focusPreviousProver app)
        | .scrollDown => some (focusNextProver app)
        | .press =>
            if mouse.button != .left || mouse.row < 2 then none
            else
              let index := mouse.row - 2
              if index < app.proverChoices.size then
                some (toggleFocusedProver (focusProver app index))
              else none
        | _ => none
  else if !app.stateOpen then
    match mouse.action with
    | .press =>
        if mouse.button != .left then none
        else
          let updated := { app with selectionStart := some (mouse.column, mouse.row) }
          let updated := { updated with selectionEnd := some (mouse.column, mouse.row) }
          let updated := { updated with copyPending := none }
          some { updated with statusNotice := none }
    | .drag =>
        if app.selectionStart.isNone || mouse.button != .left then none
        else some { app with selectionEnd := some (mouse.column, mouse.row) }
    | .release =>
        match app.selectionStart with
        | none => none
        | some _ =>
            let selected := selectedText { app with selectionEnd := some (mouse.column, mouse.row) } size
            if selected.isEmpty then none
            else
              let updated := { app with selectionEnd := some (mouse.column, mouse.row) }
              let updated := { updated with copyPending := some selected }
              some { updated with statusNotice := some s!"copied {selected.length} chars" }
    | _ => none
  else
    match drawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let contextLeft := leftWidth + 3
        let contextRight := contextLeft + rightWidth - 1
        let inContext := mouse.column >= contextLeft && mouse.column <= contextRight
        if !inContext then none
        else match mouse.action with
        | .scrollUp => some (focusPreviousContext app)
        | .scrollDown => some (focusNextContext app)
        | .press =>
            if mouse.button != .left then none
            else match contextHitAtRow app rightWidth mouse.row with
            | none => none
            | some (index, header) =>
                let app := focusContext app index
                some (if header then toggleFocusedContext app else app)
        | _ => none

private def interactive (initial : App) : IO Unit := do
  clearScreen
  TermColor.Repl.Terminal.run {
    initial
    inputConfig := inputConfig
    multiline := some multilineConfig
    fallbackSize := fallbackSize
    tickMs := 16
    mouse := true
    view := fun app size => screen app size
    complete := complete
    handleKey := handleKey
    handleMouse := handleMouse
    getState := fun app => app.repl
    setState := fun app repl => { app with repl }
    submit := submit
    jobs := some backgroundJobs
    isRunning := fun app => app.running
    quit := fun app => { app with running := false }
  }

private def runScript (app : App) (lines : List String) : IO App := do
  let mut app := app
  for line in lines do
    if app.running then
      unless line.trimAscii.toString.isEmpty do
        app ← submit app line
  pure app

private def staticOutput (app : App) : IO Unit := do
  for entry in app.entries.reverse do
    IO.println s!"[{entry.cell}] › {entry.input}"
    let timing := entry.elapsedMs.map (fun milliseconds => s!" ({formatElapsed milliseconds})") |>.getD ""
    IO.println s!"    {(if entry.ok then "=" else "!")} {entry.output}{timing}"

private def usage : String :=
  "oatp repl — interactive TPTP/Lean ATP workbench\n\n" ++
  "usage:\n  lake exe oatp repl\n  lake exe oatp repl --script FILE\n\n" ++
  "examples:\n  /load problem.p\n  /to-lean p => p\n  /snapshot\n  /to-tptp\n  /reconstruct implication-intro h exact h\n  /term"

private def scriptExitCode (app : App) : UInt32 :=
  if app.entries.all (·.ok) then 0 else 1

private def initialApp (runtime : OATP.Lean.Repl.Runtime) : IO App := do
  let (prefs, warning) ← OATP.Config.load
  let scheme := themeByName prefs.theme |>.getD aurora
  let warning := warning.orElse fun _ =>
    if themeByName prefs.theme |>.isSome then none
    else some s!"unknown configured theme `{prefs.theme}`; using {defaultThemeName}"
  pure {
    leanRuntime := some runtime
    theme := scheme
    themeName := if themeByName prefs.theme |>.isSome then prefs.theme else defaultThemeName
    theory := prefs.theory
    defaultProver := prefs.defaultProver
    enabledProvers := prefs.enabledProvers
    proverSelectionSet := prefs.proverSelectionSet
    statusNotice := warning
  }

def oatpReplMain (args : List String) : IO UInt32 := do
  if args == ["--help"] || args == ["-h"] then
    IO.println usage
    return 0
  let runtime ← OATP.Lean.Repl.create
  let initial ← initialApp runtime
  let script? ← match args with
    | ["--script", path] =>
        try pure (some (← IO.FS.readFile path))
        catch error =>
          IO.eprintln s!"could not read script `{path}`: {error}"
          return 1
    | ["--script"] =>
        IO.eprintln usage
        return 1
    | [] => pure none
    | values =>
        if values.head?.map (·.startsWith "/") |>.getD false then
          pure none
        else
          IO.eprintln s!"unknown option `{String.intercalate " " values}`\n\n{usage}"
          return 1
  let interactiveTerminal ← do
    pure ((← stdoutIsTty) && (← stdinIsTty) && (← stdoutSupportsControl))
  let blocked := (← IO.getEnv "CI").isSome || (← IO.getEnv "OATP_NONINTERACTIVE").isSome
  match script? with
  | some source =>
      let result ← runScript initial (source.splitOn "\n")
      staticOutput result
      return scriptExitCode result
  | none =>
      if interactiveTerminal && !blocked && args.isEmpty then
        let _ ← interactive initial
        return 0
      else
        let lines := if args.isEmpty then
          ["/conjecture goal p => p", "/state", "/goal p => p", "/snapshot",
             "/to-lean p => p", "/to-tptp", "/reconstruct implication-intro h exact h", "/term"]
          else [String.intercalate " " args]
        let result ← runScript initial lines
        staticOutput result
        return scriptExitCode result
