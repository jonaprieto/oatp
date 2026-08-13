/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP
import OATP.ReplView
import TermColor.Diagnostics
import TermColor.Repl.Command
import TermColor.Repl.Terminal
import TermColor.Terminal

/-!
# oatp repl

The interactive shell combines the pure TPTP session model with OATP's existing process, portfolio,
catalogue, and Lean proof boundaries. Static script mode keeps the same command path testable in CI.
-/

open OATP
open OATP.Repl
open OATP.ReplView
open Lean
open TermColor
open TermColor.Diagnostics
open TermColor.Repl
open TermColor.Terminal
open TermColor.Widgets

private def catalogueNamespace : String := "oatp-repl"

private def appendEntry (app : App) (cell : Nat) (input output : String) (ok : Bool)
    (elapsedMs : Option Nat := none) (sources : Sources := #[])
    (diagnostic : Option Diagnostic := none) (report : Option Report := none) : App :=
  { app with
    entries := { cell, input, output, ok, elapsedMs, sources, diagnostic, report } :: app.entries
    transcriptScroll := 0
    repl := { app.repl with input := {}, historyIndex := none, completion := none } }

#guard (appendEntry { repl := { history := #["first"] } } 1 "input" "output" true).repl.history ==
  #["first"]

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

private def notePlain (app : App) (cell : Nat) (input output : String) (ok : Bool)
    (elapsedMs : Option Nat := none) (report : Option Report := none) : App :=
  appendEntry { app with session := OATP.Repl.note app.session input output } cell input output ok
    elapsedMs #[] none report

#guard match (notePlain { } 1 "/run eprover" "eprover: Error" false).entries with
  | entry :: _ => match entry.diagnostic with | none => true | some _ => false
  | [] => false

private def noteDiagnostic (app : App) (cell : Nat) (input source message : String) : App :=
  let (source, diagnostic) := diagnosticFor source message
  appendEntry { app with session := OATP.Repl.note app.session input message }
    cell input message false
    none #[source] (some diagnostic)

private def clearDerived (app : App) : App :=
  { app with goal := none, translation := none, term := none, leanGoal := none }

private def invalidateContext (app : App) : App :=
  { clearDerived app with
    runRows := #[]
    runOpen := false
    runFocus := 0
    contextItemFocus := none }

private def clearTranscript (app : App) : App :=
  { app with
    entries := []
    transcriptScroll := 0
    historyExpanded := #[]
    selectionStart := none
    selectionEnd := none
    copyPending := none
    repl := { app.repl with input := {}, historyIndex := none, completion := none } }

private def changesContext (input : String) : Bool :=
  match OATP.Repl.parseInput input with
  | .source _ => true
  | .command command => match command with
      | .parse _ | .axiom _ _ | .conjecture _ _ | .remove _ | .update _ _ | .reset => true
      | _ => false

private def lastHistory (session : OATP.Repl.Session) : String :=
  session.history.toList.reverse.head?.map (·.result) |>.getD "ok"

private def artifactText : Portfolio.Result → (String × Bool)
  | .artifact attempt artifact =>
      let head := s!"{attempt.name}: {artifact.status} ({artifact.elapsedMs}ms)"
      let details := if artifact.stderr.contains "no input problem files" then
          "\n  this prover needs an input file; try `/check` or `/run --prover eprover`"
        else
          let stdout := if artifact.stdout.isEmpty then "" else
            "\n" ++ artifact.stdout.trimAscii.toString
          let stderr := if artifact.stderr.isEmpty then "" else
            "\nstderr: " ++ artifact.stderr.trimAscii.toString
          stdout ++ stderr
      (head ++ details, SZSStatus.isSuccess artifact.status)
  | .failed attempt failure => (s!"{attempt.name}: failed: {failure.message}", false)

private def resultHasIssue : Portfolio.Result → Bool
  | .artifact _ artifact => !SZSStatus.isSuccess artifact.status
  | .failed _ _ => true

private def resultSucceeded : Portfolio.Result → Bool
  | .artifact _ artifact => SZSStatus.isSuccess artifact.status
  | .failed _ _ => false

private def resultIssueText : Portfolio.Result → String
  | .artifact attempt artifact => s!"{attempt.name}: returned {artifact.status}"
  | .failed attempt failure => s!"{attempt.name}: {failure.message}"

private def runReport (strategy : OATP.RunStrategy) (results : List Portfolio.Result) : Report :=
  let issues := results.filter resultHasIssue
  let report := if strategy == .firstSuccess && results.any resultSucceeded then
      Report.info "first successful prover completed"
    else if issues.isEmpty then
      Report.info "all selected provers completed"
    else
      Report.error "some prover results need attention"
  let report := report.withCode "check"
    |>.withField "provers" (toString results.length)
    |>.withField "issues" (toString issues.length)
    |>.withHelp "click to expand; Ctrl-R opens the full run output"
  issues.foldl (fun report result => report.withNote (resultIssueText result)) report

structure RunOutput where
  text : String
  ok : Bool
  report : Report

private def failedRunOutput (code message : String) : RunOutput :=
  { text := message, ok := false, report := (Report.error message).withCode code }

private def runRow : Portfolio.Result → RunRow
  | .artifact attempt artifact =>
      { name := attempt.name
        status := .result artifact.status
        detail := if artifact.stderr.contains "no input problem files" then
            "this prover needs an input file; try `/check` or `/run --prover eprover`"
          else if !artifact.stderr.isEmpty then artifact.stderr.trimAscii.toString
          else if !artifact.stdout.isEmpty then artifact.stdout.trimAscii.toString
          else s!"completed with {artifact.status}"
        elapsedMs := some artifact.elapsedMs }
  | .failed attempt failure =>
      { name := attempt.name, status := .failed, detail := failure.message }

private def runRowsFor (attempts : Array Portfolio.Attempt) (strategy : OATP.RunStrategy) :
    Array RunRow :=
  attempts.mapIdx fun index attempt => {
    name := attempt.name
    status := if strategy == .all || index == 0 then .running else .queued }

private def finishRunRows (rows : Array RunRow) (results : List Portfolio.Result)
    (strategy : OATP.RunStrategy) : Array RunRow :=
  let rows := results.foldl (fun rows result =>
    let completed := runRow result
    rows.map fun current => if current.name == completed.name then completed else current) rows
  if strategy == .firstSuccess && results.any resultSucceeded then
    rows.map fun row => if row.status == .queued then
      { row with status := .cancelled, detail := "skipped after first success" } else row
  else rows

private def publishRunRows (app : App) (rows : Array RunRow) : IO Unit := do
  match app.runProgress with
  | some progress => progress.set rows
  | none => pure ()

private def currentRunRows (app : App) : IO (Array RunRow) := do
  match app.runProgress with
  | some progress => progress.get
  | none => pure app.runRows

private def currentProblem (app : App) : Option Problem :=
  app.translation.map (fun source => { name := "lean-goal", source }) |>.orElse
    (fun _ => OATP.Repl.problem app.session)

private def theoryStatus (theory : String) : String :=
  s!"theory: {theory}; available: {String.intercalate ", " OATP.TPTP.supportedTheories}"

#guard theoryStatus "fof" == "theory: fof; available: fof, cnf, tff"

private def preferences (app : App) : OATP.Config.Preferences := {
  theory := app.theory
  defaultProver := app.defaultProver.map OATP.ProverReference.persisted |>.getD ""
  enabledProvers := app.enabledProvers.toList.map OATP.ProverReference.persisted |>.toArray
  proverSelectionSet := app.proverSelectionSet
  strategy := OATP.RunStrategy.name app.strategy
  theme := app.themeName
}

private def configProverName (value : String) : String :=
  OATP.ProverReference.fromPersisted value |>.map OATP.ProverReference.display |>.getD value

private def configOutput (app : App) : IO (String × Report) := do
  let path := (← OATP.Config.path).map (fun value => s!"{value}") |>.getD "unavailable"
  let (_, warning) ← OATP.Config.load
  let current := preferences app
  let selected := if !current.proverSelectionSet then
      "automatic"
    else if current.enabledProvers.isEmpty then
      "none"
    else
      String.intercalate ", " (current.enabledProvers.toList.map configProverName)
  let default := if current.defaultProver.isEmpty then "auto"
    else configProverName current.defaultProver
  let report := match warning with
    | some _ => Report.warning "REPL configuration (using a fallback)"
    | none => Report.info "REPL configuration"
  let report := report.withCode "config"
    |>.withField "path" path
    |>.withField "consumer" "REPL startup"
    |>.withField "theory" current.theory
    |>.withField "default prover" default
    |>.withField "selected provers" selected
    |>.withField "strategy" current.strategy
    |>.withField "theme" current.theme
    |>.withHelp "configuration is read at REPL startup"
  let output := String.intercalate "\n" <| [
    "CONFIGURATION",
    s!"  path:             {path}",
    "  consumer:         REPL startup",
    s!"  theory:           {current.theory}",
    s!"  default prover:   {default}",
    s!"  selected provers: {selected}",
    s!"  strategy:         {current.strategy}",
    s!"  theme:            {current.theme}"] ++ match warning with
      | some message => ["  warning:          " ++ message]
      | none => []
  pure (output, report)

private def savePreferences (before after : App) : IO App := do
  if preferences before == preferences after then
    pure after
  else
    match ← OATP.Config.save (preferences after) with
    | none => pure after
    | some message => pure { after with statusNotice := some message }

private def installedProverReferences : IO (Array OATP.ProverReference) := do
  let installed ← OATP.Runtime.installedProvers
  pure (installed.map OATP.ProverReference.fromLocal)

private def onlineProverNames : IO (Except String (Array OATP.ProverReference)) := do
  match ← OATP.Runtime.loadCatalogue catalogueNamespace
      SystemOnTPTP.defaultCatalogueEndpoint .normal with
  | .error message => pure (.error message)
  | .ok systems => pure (.ok (systems.map fun system => OATP.ProverReference.fromOnline system.id))

private def selectableProvers : IO (Array OATP.ProverReference × Option String) := do
  let installed ← installedProverReferences
  match ← onlineProverNames with
  | .ok online => pure (installed ++ online, none)
  | .error message => pure (installed, some message)

private def defaultProvers (app : App) : IO (Array OATP.ProverReference) := do
  let installed ← installedProverReferences
  match app.defaultProver with
  | some reference => match reference.kind with
      | .online =>
          match ← onlineProverNames with
          | .ok online => pure <| if online.any (· == reference) then #[reference] else #[]
          | .error _ => pure #[]
      | .local => pure <| if installed.any (· == reference) then #[reference] else #[]
  | none => match OATP.Runtime.defaultLocalProver (installed.map (·.name)) with
      | some name => pure #[OATP.ProverReference.fromLocal name]
      | none => pure #[]

private def selectedProvers (app : App) (all includeDefault : Bool := false) :
    IO (Array OATP.ProverReference) := do
  let installed ← installedProverReferences
  let selected ← if all then
      pure installed
    else if app.proverSelectionSet then
    if app.enabledProvers.any (fun reference => reference.kind == .online) then
      match ← onlineProverNames with
      | .ok online =>
          let available := installed ++ online
          pure <| app.enabledProvers.filter fun reference =>
            available.any (· == reference)
      | .error _ => pure <| app.enabledProvers.filter fun reference =>
          installed.any (· == reference)
    else
      pure <| app.enabledProvers.filter (fun reference => installed.any (· == reference))
    else
      defaultProvers app
  if !includeDefault then
    pure selected
  else
    let defaults ← defaultProvers app
    pure <| defaults.foldl (fun selected reference =>
      if selected.any (· == reference) then selected else selected.push reference) selected

private def runRequest (app : App) (request : OATP.Repl.RunRequest)
    (includeDefault : Bool := false) : IO RunOutput := do
  match currentProblem app with
  | none =>
      let message :=
        "no current problem; try `/parse fof(goal, conjecture, p => p).` or `/load FILE`"
      publishRunRows app #[{ name := "run", status := .failed, detail := message }]
      pure { text := message, ok := false, report := (Report.error message).withCode "check" }
  | some problem =>
      let references ← if request.references.isEmpty then
        pure (← selectedProvers app request.all includeDefault).toList
      else pure request.references
      let localReferences := references.filterMap fun reference =>
        match reference.kind with
        | .local => some reference.name
        | .online => none
      let onlineReferences := references.filterMap fun reference =>
        match reference.kind with
        | .local => none
        | .online => some reference.name
      let limits : Limits := { wallSeconds := request.timeout, maxOutputBytes := request.maxOutput }
      let mut attempts : Array Portfolio.Attempt := #[]
      for reference in localReferences do
        unless attempts.any (·.name == reference) do
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
        match ← OATP.Runtime.loadCatalogue catalogueNamespace endpoint mode with
        | .error message =>
            let row : RunRow :=
              { name := "online catalogue"
                status := .failed
                detail := message }
            publishRunRows app #[row]
            return failedRunOutput "catalogue" message
        | .ok systems =>
            match OATP.Runtime.resolveOnline catalogueNamespace systems onlineReferences with
            | .error message =>
                let row : RunRow :=
                  { name := "online prover"
                    status := .failed
                    detail := message }
                publishRunRows app #[row]
                return failedRunOutput "prover" message
            | .ok resolved =>
                let labels := resolved.map (·.id)
                let commands := resolved.toList.filterMap fun system =>
                  if system.command.isEmpty then none else some (system.id, system.command)
                if app.strategy == .firstSuccess then
                  for system in resolved do
                    attempts := attempts.push {
                      name := "online " ++ system.id
                      limits
                      backend := .online {
                        endpoint
                        systemLabel := system.id
                        systemLabels := #[system.id]
                        systemCommands := if system.command.isEmpty then #[] else #[
                          (system.id, system.command)]
                        timeLimit := request.timeout
                        maxBodyBytes := request.maxOutput
                      }
                    }
                else
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
        let message := "no prover selected or installed"
        publishRunRows app #[{ name := "run", status := .failed, detail := message }]
        pure { text := message, ok := false, report := (Report.error message).withCode "check" }
      else
        publishRunRows app (runRowsFor attempts app.strategy)
        let results ← Portfolio.runWithStrategy problem attempts app.strategy fun result => do
          match app.runProgress with
          | some progress =>
              let row := runRow result
              let rows ← progress.get
              let rows := rows.map fun current =>
                if current.name == row.name then row else current
              let rows := if app.strategy == .firstSuccess && !resultSucceeded result then
                  match rows.toList.findIdx? (·.status == .queued) with
                  | some index => rows.mapIdx fun currentIndex row =>
                      if currentIndex == index then { row with status := .running } else row
                  | none => rows
                else rows
              progress.set rows
          | none => pure ()
        let rendered := results.toList.map artifactText
        publishRunRows app (finishRunRows (← currentRunRows app) results.toList app.strategy)
        pure <| RunOutput.mk (String.intercalate "\n" (rendered.map Prod.fst))
          (rendered.any Prod.snd) (runReport app.strategy results.toList)

private def backendLine (input : String) : Bool :=
  match OATP.Repl.parseCommandSpec input with
  | .ok command => match command with
      | .run _ | .local _ | .online _ | .check => true
      | _ => false
  | .error _ => false

private def backendRequest (input : String) :
    Except String (OATP.Repl.RunRequest × Bool) :=
  match OATP.Repl.parseCommandSpec input with
  | .ok (.run request) => pure (request, false)
  | .ok (.local request) => pure ({
      references := [OATP.ProverReference.fromLocal request.executable]
      timeout := request.timeout
      maxOutput := request.maxOutput
      arguments := request.arguments }, false)
  | .ok (.online request) => pure ({
      references := [OATP.ProverReference.fromOnline request.system]
      endpoint := request.endpoint
      timeout := request.timeout
      maxOutput := request.maxOutput }, false)
  | .ok .check => pure ({}, true)
  | .ok _ => .error "not a prover command"
  | .error message => .error message

private def startingRunRows (input : String) : Array RunRow :=
  match backendRequest input with
  | .ok (request, includeDefault) =>
      if request.references.isEmpty then #[{ name := if includeDefault then
          "default + selected provers" else "selected provers", status := .running }]
      else request.references.toArray.map fun reference =>
        { name := OATP.ProverReference.display reference, status := .running }
  | .error _ => #[{ name := "run", status := .running }]

private def backgroundJobs : TermColor.Repl.Terminal.JobConfig App where
  shouldRun := fun app input => !app.busy && backendLine input
  start := fun app input => { app with
    busy := true
    jobResult := none
    runOpen := (currentProblem app).isSome
    stateOpen := false
    historyOpen := false
    proversOpen := false
    panelFocus := .main
    runFocus := 0
    runRows := startingRunRows input
    repl := { app.repl with input := {}, historyIndex := none, completion := none } }
  tick := fun app => do
    let rows ← currentRunRows app
    pure { app with
      runRows := if rows.isEmpty then app.runRows else rows
      runFrame := app.runFrame + 1 }
  run := fun cancellation app input => do
    let started ← IO.monoMsNow
    unless ← TermColor.Repl.Terminal.Cancellation.sleep cancellation 1 do
      return app
    match backendRequest input with
    | .error message =>
        pure { app with jobResult := some {
          cell := app.session.nextCell, input, output := message, ok := false,
          elapsedMs := some ((← IO.monoMsNow) - started),
          report := some ((Report.error message).withCode "command") } }
    | .ok (request, includeDefault) =>
        -- partiality: portfolio execution has no process-cancellation seam yet; keep the UI
        -- responsive and add cancellation at Portfolio once process ownership is exposed.
        let result ← runRequest app request includeDefault
        let rows ← currentRunRows app
        pure { app with runRows := rows, jobResult := some {
          cell := app.session.nextCell, input, output := result.text, ok := result.ok,
          elapsedMs := some ((← IO.monoMsNow) - started), report := some result.report } }
  finish := fun current completed =>
    match completed.jobResult with
    | some result =>
        notePlain { current with busy := false, jobResult := none }
          result.cell result.input result.output result.ok result.elapsedMs result.report
    | none => { current with busy := false }
  cancel := fun app => { app with
    busy := false
    jobResult := none
    runRows := app.runRows.map fun row => { row with status := .cancelled, detail := "cancelled" } }
  fail := fun app message =>
    let updated := { app with
      busy := false
      jobResult := none
      runRows := app.runRows.map fun row => { row with status := .failed, detail := message } }
    notePlain updated app.session.nextCell "background prover" message false none
      (some ((Report.error message).withCode "background"))

#guard (backgroundJobs.start { repl := { history := #["first"] } } "/help").repl.history ==
  #["first"]

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
    match ← OATP.Runtime.loadCatalogue catalogueNamespace endpoint mode with
    | .error message => pure <| String.intercalate "\n" lines ++ "\nONLINE: " ++ message
    | .ok systems =>
        pure <| String.intercalate "\n" (lines ++ ["ONLINE:"] ++
          systems.toList.map (fun system => "  " ++ SystemOnTPTP.onlineReference system.id))

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

private def proverMatches (query : String) (reference : OATP.ProverReference) : Bool :=
  fuzzy query (OATP.ProverReference.display reference) || fuzzy query reference.name

private def infoText (app : App) (query : String) : IO (String × Bool) := do
  let installed ← OATP.Runtime.installedProvers
  let found := installed.filter (fuzzy query)
  if found.size == 1 then
    let name := found[0]!
    let reference := OATP.ProverReference.fromLocal name
    let version := (← Http.commandVersion name).getD "not installed"
    let enabled := if app.proverSelectionSet then
        app.enabledProvers.any (· == reference)
      else true
    return (String.intercalate "\n" [
      s!"prover: {name}",
      s!"kind:   local executable",
      s!"version: {version}",
      s!"default: {if app.defaultProver == some reference then "yes" else "no"}",
      s!"enabled: {if enabled then "yes" else "no"}"
    ], true)
  if found.size > 1 then
    return (s!"`{query}` matches local provers: {String.intercalate ", " found.toList}", false)
  let endpoint := SystemOnTPTP.defaultCatalogueEndpoint
  match ← OATP.Runtime.loadCatalogue catalogueNamespace endpoint .normal with
  | .error message => pure (String.intercalate "\n" [
      s!"no local prover matched `{query}`",
      s!"online catalogue unavailable: {message}",
      s!"installed local provers: {if installed.isEmpty then "none" else
        String.intercalate ", " installed.toList}"
    ], false)
  | .ok systems =>
      let online := systems.filter (fun system =>
        fuzzy query (SystemOnTPTP.onlineReference system.id) ||
          fuzzy query system.id || fuzzy query (SystemOnTPTP.Catalogue.baseName system.id))
      match online.toList with
      | [] => pure (String.intercalate "\n" [
          s!"no local or online prover matched `{query}`",
          s!"installed local provers: {if installed.isEmpty then "none" else
            String.intercalate ", " installed.toList}"
        ], false)
      | [system] => pure (String.intercalate "\n" [
          s!"prover: {SystemOnTPTP.onlineReference system.id}",
          "kind:   online SystemOnTPTP prover",
          "local:  not installed",
          s!"system: {system.id}",
          s!"command: {if system.command.isEmpty then "catalogue default" else system.command}",
          s!"time limit: {system.timeLimit}s"
        ], true)
      | _ => pure (s!"`{query}` matches online provers: {
          String.intercalate ", " (online.toList.map (fun system =>
            SystemOnTPTP.onlineReference system.id))}", false)

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
  let (step, rest) ← parseStepTokens (OATP.Repl.splitWords source)
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

private def submitTheoryFormula (app : App) (cell : Nat) (input : String)
    (command : OATP.Repl.Command) : IO (Option App) := do
  match command with
  | .axiom name formula | .conjecture name formula =>
      let role := match command with | .axiom _ _ => "axiom" | _ => "conjecture"
      let source := s!"{app.theory}({name}, {role}, {formula})."
      match OATP.Repl.parseSource app.session input source with
      | .ok session => pure (some (note (invalidateContext { app with session }) cell input
          s!"parsed 1 statement using {app.theory}" true))
      | .error message => pure (some (note app cell input message false))
  | _ => pure none

private def submitLeanCommand (app : App) (cell : Nat) (input : String)
    (command : OATP.Repl.Command) : IO (Option App) := do
  match command with
  | .goal formula | .toLean formula => do
      return some (← submitGoal app cell input formula)
  | .snapshot => do
    match app.leanRuntime, app.leanGoal with
    | some runtime, some goal =>
        let (runtime, snapshot) ← OATP.Lean.Repl.snapshot runtime goal
        let context := if snapshot.context.isEmpty then "(no local hypotheses)" else
          String.intercalate "\n" snapshot.context.toList
        let output := "Lean goal snapshot\n" ++ context ++ "\n⊢ " ++ snapshot.target
        return some <| note { app with leanRuntime := some runtime, goal := some snapshot }
          cell input output true
    | _, _ => return some (note app cell input "no Lean goal" false)
  | .toTptp => do
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
  | .reconstruct source => do
    match app.leanRuntime, app.leanGoal, parseStep source with
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
  | .term =>
    match app.term with
    | some term => return some (note app cell input s!"{term.term}\n: {term.type}" true)
    | none => return some (note app cell input "no rendered term" false)
  | _ => pure none

private def applyPureCommand (app : App) (cell : Nat) (input : String) : IO App := do
  match OATP.Repl.apply app.session input with
  | .ok session =>
      let app := if changesContext input then invalidateContext { app with session }
        else { app with session }
      if input == "/clear" || input == "/reset" then
        pure (clearTranscript app)
      else
        pure <| appendEntry app cell input (lastHistory session) true
  | .error message => pure (note app cell input message false)

private def submitCommand (app : App) (cell : Nat) (input : String)
    (command : OATP.Repl.Command) : IO App := do
  match command with
  | .quit => pure { app with running := false }
  | .load path => do
      try
        let source ← IO.FS.readFile path
        match OATP.Repl.parseSource app.session input source with
        | .ok session => pure (appendEntry (invalidateContext { app with session }) cell input
            (lastHistory session) true)
        | .error message => pure (note app cell input message false)
      catch error => pure (note app cell input s!"could not read `{path}`: {error}" false)
  | .axiom _ _ | .conjecture _ _ => do
      match ← submitTheoryFormula app cell input command with
      | some app => pure app
      | none => pure (note app cell input "invalid formula command" false)
  | .goal _ | .toLean _ | .snapshot | .toTptp | .reconstruct _ | .term => do
      match ← submitLeanCommand app cell input command with
      | some app => pure app
      | none => pure (note app cell input "invalid Lean command" false)
  | .state =>
      let updated := toggleStatePanel app
      let message := if updated.panelFocus == .drawer then
          "state drawer focused"
        else "state drawer closed"
      let session := OATP.Repl.note app.session input message
      pure (appendEntry { updated with session } cell input message true)
  | .strategy none => pure (note app cell input
      s!"strategy: {OATP.RunStrategy.name app.strategy}; available: {
        String.intercalate ", " OATP.RunStrategy.choices}" true)
  | .strategy (some value) =>
      match OATP.RunStrategy.ofString value with
      | some strategy => pure (note { app with strategy } cell input
          s!"strategy set to {OATP.RunStrategy.name strategy}" true)
      | none => pure (note app cell input s!"unknown strategy `{value}`; try: {
          String.intercalate ", " OATP.RunStrategy.choices}" false)
  | OATP.Repl.Command.stateTarget stateName =>
      match openContextTarget
          { app with
            historyOpen := false
            proversOpen := false
            runOpen := false
            panelFocus := .drawer } stateName with
      | some focused => pure (note focused cell input s!"state: {stateName}" true)
      | none => pure (note app cell input
          s!"unknown state target `{stateName}`; try: {
            String.intercalate ", " contextTargetNames}" false)
  | .history =>
      let session := OATP.Repl.note app.session input "history drawer toggled"
      pure (appendEntry
        { app with
          session := session
          historyOpen := !app.historyOpen
          stateOpen := false
          proversOpen := false
          runOpen := false
          panelFocus := if app.historyOpen then .main else .drawer }
        cell input "history drawer toggled" true)
  | .theme none => pure (note app cell input
      s!"theme: {app.themeName}; available: {themeNames}" true)
  | .theme (some name) =>
      match themeByName name with
      | some scheme => pure (note { app with theme := scheme, themeName := name } cell input
          s!"theme changed to {name}" true)
      | none => pure (note app cell input s!"unknown theme `{name}`; try: {themeNames}" false)
  | .theory none => pure (note app cell input (theoryStatus app.theory) true)
  | .theory (some requested) =>
      let requested := requested.toLower
      match OATP.TPTP.normalizeTheory requested with
      | some theory =>
        pure (note { app with theory } cell input s!"theory set to {theory}" true)
      | none => pure (note app cell input s!"unknown theory; use {
          String.intercalate ", " OATP.TPTP.theoryChoices}" false)
  | .prover none => pure (note app cell input
      s!"default prover: {app.defaultProver.map OATP.ProverReference.display |>.getD "auto"}" true)
  | .prover (some name) => do
      let (candidates, warning) ← selectableProvers
      let found := candidates.filter (proverMatches name)
      match found.toList with
      | [resolved] =>
          let updated := note { app with defaultProver := resolved } cell input
            s!"default prover set to {OATP.ProverReference.display resolved}" true
          pure <| match warning with
            | none => updated
            | some message => { updated with statusNotice := some s!"online catalogue: {message}" }
      | [] => pure (note app cell input s!"unknown prover `{name}`" false)
      | _ => pure (note app cell input (s!"ambiguous prover `{name}`: " ++
          String.intercalate ", " (found.toList.map OATP.ProverReference.display)) false)
  | .provers => do
      let installed ← installedProverReferences
      let (choices, warning) ← selectableProvers
      let defaultEnabled := match installed[0]? with
        | some prover => #[prover]
        | none => #[]
      let enabled := if !app.proverSelectionSet then defaultEnabled else
        app.enabledProvers.filter (fun name => choices.any (· == name))
      let onlineCount := choices.size - min choices.size installed.size
      let output := if choices.isEmpty then "no local or online provers"
        else s!"prover drawer ready: {installed.size} local, {onlineCount} online; " ++
          "online provers start unchecked; use J/K and Space to toggle"
      let output := match warning with
        | none => output
        | some message => output ++ "\nONLINE: catalogue unavailable: " ++ message
      let updated := { app with proverChoices := choices }
      let updated := { updated with enabledProvers := enabled }
      let updated := { updated with proverSelectionSet := true }
      let updated := { updated with proversOpen := !app.proversOpen }
      let updated := { updated with
        stateOpen := false
        historyOpen := false
        runOpen := false
        panelFocus := if app.proversOpen then .main else .drawer }
      pure (note updated cell input output true)
  | .info query => do
      let (output, ok) ← infoText app query
      pure (note app cell input output ok)
  | .systems request => do
      let output ← systemsText request
      pure (note app cell input output true)
  | .doctor => do
      let output ← doctorText
      pure (note app cell input output true)
  | .config => do
      let (output, report) ← configOutput app
      pure (notePlain app cell input output true none (some report))
  | .run request => do
      let result ← runRequest app request
      pure (notePlain app cell input result.text result.ok none (some result.report))
  | .local request => do
      let run : OATP.Repl.RunRequest := {
        references := [OATP.ProverReference.fromLocal request.executable]
        timeout := request.timeout
        maxOutput := request.maxOutput
        arguments := request.arguments }
      let result ← runRequest app run
      pure (notePlain app cell input result.text result.ok none (some result.report))
  | .online request => do
      let run : OATP.Repl.RunRequest := {
        references := [OATP.ProverReference.fromOnline request.system]
        endpoint := request.endpoint
        timeout := request.timeout
        maxOutput := request.maxOutput }
      let result ← runRequest app run
      pure (notePlain app cell input result.text result.ok none (some result.report))
  | .check => do
      let result ← runRequest app {} true
      pure (notePlain app cell input result.text result.ok none (some result.report))
  | _ => applyPureCommand app cell input

private def submitCore (app : App) (input : String) : IO App := do
  let input := input.trimAscii.toString
  let cell := app.session.nextCell
  let firstWord := (OATP.Repl.splitWords input).headD ""
  if app.busy && backendLine input then
    return note app cell input "a prover run is already active; inspect it with Ctrl-R" false
  if app.busy && changesContext input then
    return note app cell input "finish the active prover run before changing context" false
  if !input.startsWith "/" && OATP.Repl.commandNames.any (· == firstWord) then
    return note app cell input "commands start with `/`; try `/help`" false
  if input.startsWith "/" then
    match OATP.Repl.parseCommandSpec input with
    | .ok command => return (← submitCommand app cell input command)
    | .error message => return note app cell input message false
  else
    return (← applyPureCommand app cell input)

private def submit (app : App) (input : String) : IO App := do
  let started ← IO.monoMsNow
  let updated ← submitCore app input
  let updated := clearSelection updated
  let app ← savePreferences app updated
  let elapsedMs := (← IO.monoMsNow) - started
  match app.entries with
  | entry :: rest => pure { app with entries := { entry with elapsedMs := some elapsedMs } :: rest }
  | [] => pure app

private def commandValues (typeName : String) : IO (List String) := do
  match typeName with
  | "TOPIC" => pure (List.eraseDups (OATP.Repl.helpTopics ++ OATP.Repl.commandNames))
  | "FORMAT" | "THEORY" | "STEP" =>
      pure (OATP.Repl.staticCompletionValues typeName)
  | "STRATEGY" => pure OATP.RunStrategy.choices
  | "TARGET" => pure (contextTargetNames ++ ["all"])
  | "THEME" => pure (themes.map Prod.fst)
  | "PROVER" =>
      let localNames ← OATP.Runtime.localProverCandidates
      let online ← match ← onlineProverNames with
        | .ok names => pure names
        | .error _ => pure #[]
      pure (localNames.toList ++ online.toList.map OATP.ProverReference.display)
  | "SYSTEM" =>
      let localNames ← OATP.Runtime.localProverCandidates
      let online ← match ← onlineProverNames with
        | .ok names => pure <| names.map (·.name)
        | .error _ => pure #[]
      pure (localNames.toList ++ online.toList)
  | _ => pure []

private def complete (_app : App) (input : TextInputState) : IO (List Completion) :=
  completeCommandWith OATP.Repl.commandSpec commandValues input

private def appKeymap : TermColor.Repl.Terminal.AppKeymap App where
  Action := AppKeyAction
  keymap := Keymap.fromSpecs appBindings
  contexts := fun app =>
    let drawerContexts := if app.runOpen then
        [AppContext.keyContext .run, AppContext.keyContext .runInput]
      else if app.proversOpen then
        [AppContext.keyContext .provers, AppContext.keyContext .proversInput]
      else if app.stateOpen then
        [AppContext.keyContext .state, AppContext.keyContext .stateInput]
    else if app.historyOpen then
        [AppContext.keyContext .history, AppContext.keyContext .historyInput]
      else []
    if app.panelFocus == .drawer then drawerContexts
    else [AppContext.keyContext .default]
  handle := fun app action => some (clearSelection (match action with
    | .toggleRun => toggleRunPanel app
    | .toggleHistory => toggleHistoryPanel app
    | .toggleState => toggleStatePanel app
    | .focusDrawer =>
        if app.runOpen || app.stateOpen || app.historyOpen || app.proversOpen then
          { app with panelFocus := .drawer }
        else app
    | .closeRun => { app with runOpen := false, panelFocus := .main }
    | .runNext => focusNextRun app
    | .runPrevious => focusPreviousRun app
    | .runInspect => focusRun app app.runFocus
    | .closeProvers => { app with proversOpen := false, panelFocus := .main }
    | .proverNext => focusNextProver app
    | .proverPrevious => focusPreviousProver app
    | .toggleProver => toggleFocusedProver app
    | .closeState => { app with panelFocus := .main }
    | .contextNext => focusNextContextEntry app
    | .contextPrevious => focusPreviousContextEntry app
    | .prepareRemoveContext => removeContextItem app
    | .prepareUpdateContext => editContextItem app
    | .toggleContext => toggleFocusedContext app
    | .expandContext => expandFocusedContext app
    | .collapseContext => collapseFocusedContext app
    | .closeHistory => { app with panelFocus := .main }
      | .transcriptPageUp => scrollTranscriptPageUp app
    | .transcriptPageDown => scrollTranscriptPageDown app))

#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .state,
    AppContext.keyContext .stateInput] .escape ==
  some AppKeyAction.closeState
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .history,
    AppContext.keyContext .historyInput] .escape ==
  some AppKeyAction.closeHistory
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .default,
    AppContext.keyContext .stateInput] .escape == some AppKeyAction.closeState
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .default,
    AppContext.keyContext .stateInput] (.char 'j') == none
#guard appKeymap.contexts {} == [AppContext.keyContext .default]
#guard (Keymap.fromSpecs appBindings).conflicts == []
#guard appKeyLabel .toggleRun .default == "Ctrl-R/Ctrl-r"
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .default] (.ctrl 'h') ==
  some AppKeyAction.toggleHistory
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .default] (.ctrl 's') ==
  some AppKeyAction.toggleState
#guard match backgroundJobs.start ({} : App) "/run" with
  | app => app.busy && !app.runOpen && app.panelFocus == .main
#guard (removeContextItem { contextItemFocus := some 2 } : App).repl.input.value == "/remove #2"
#guard (editContextItem { contextItemFocus := some 2 } : App).repl.input.value == "/update #2 "
#guard (removeContextItem { contextItemFocus := some 2 } : App).panelFocus == .main
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .state] .delete ==
  some AppKeyAction.prepareRemoveContext
#guard (Keymap.fromSpecs appBindings).resolve [AppContext.keyContext .state] (.char 'e') ==
  some AppKeyAction.prepareUpdateContext

private def handleMouse (app : App) (size : Size) (mouse : MouseEvent) : Option App :=
  if app.runOpen then
    match drawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let drawerLeft := leftWidth + 3
        let drawerRight := drawerLeft + rightWidth - 1
        if mouse.column < drawerLeft then
          match mouse.action with
          | .press =>
              if mouse.button != .left then none
              else match reportAtScreenRow app { size with columns := leftWidth } mouse.row with
              | some cell => some (clearSelection (toggleReportCell app cell))
              | none => some { app with panelFocus := .main }
          | _ => some { app with panelFocus := .main }
        else if mouse.column > drawerRight then
          some { app with panelFocus := .main }
        else match mouse.action with
        | .scrollUp => some (focusPreviousRun { app with panelFocus := .drawer })
        | .scrollDown => some (focusNextRun { app with panelFocus := .drawer })
        | .press =>
            if mouse.button != .left || mouse.row < 2 then none
            else
              let index := mouse.row - 2
              if index < app.runRows.size then
                some (focusRun { app with panelFocus := .drawer } index)
              else none
        | _ => none
  else if app.proversOpen then
    match drawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let contextLeft := leftWidth + 3
        let contextRight := contextLeft + rightWidth - 1
        if mouse.column < contextLeft || mouse.column > contextRight then
          some { app with panelFocus := .main }
        else match mouse.action with
        | .scrollUp => some (focusPreviousProver { app with panelFocus := .drawer })
        | .scrollDown => some (focusNextProver { app with panelFocus := .drawer })
        | .press =>
            if mouse.button != .left || mouse.row < 2 then none
            else
              let index := proverVisibleStart app size.rows + mouse.row - 2
              if index < app.proverChoices.size then
                some (toggleFocusedProver (focusProver { app with panelFocus := .drawer } index))
              else none
        | _ => none
  else if app.panelFocus == .main then
    match mouse.action with
    | .scrollUp => some (clearSelection (scrollTranscriptUp app))
    | .scrollDown => some (clearSelection (scrollTranscriptDown app))
    | .press =>
        if mouse.button != .left then none
        else
          match reportAtScreenRow app size mouse.row with
          | some cell => some (clearSelection (toggleReportCell app cell))
          | none =>
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
            let selected := selectedText
              { app with selectionEnd := some (mouse.column, mouse.row) } size
            if selected.isEmpty then none
            else
              let updated := { app with selectionEnd := some (mouse.column, mouse.row) }
              let updated := { updated with copyPending := some selected }
              some { updated with statusNotice := some s!"copied {selected.length} chars" }
  else if app.historyOpen then
    match drawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let panelLeft := leftWidth + 3
        let panelRight := panelLeft + rightWidth - 1
        if mouse.column < panelLeft || mouse.column > panelRight then
          some { app with panelFocus := .main }
        else match mouse.action with
        | .press =>
            if mouse.button != .left || mouse.row < 2 then none
            else match historyCellAtRow app rightWidth (mouse.row - 2) with
            | some cell => some (toggleHistoryCell { app with panelFocus := .drawer } cell)
            | none => none
        | _ => some app
  else
    match drawerWidths size.columns with
    | none => none
    | some (leftWidth, rightWidth) =>
        let contextLeft := leftWidth + 3
        let contextRight := contextLeft + rightWidth - 1
        let inContext := mouse.column >= contextLeft && mouse.column <= contextRight
        if !inContext then
          match mouse.action with
          | .scrollUp => some (clearSelection (scrollTranscriptUp { app with panelFocus := .main }))
          | .scrollDown =>
              some (clearSelection (scrollTranscriptDown { app with panelFocus := .main }))
          | _ => some { app with panelFocus := .main }
        else match mouse.action with
        | .scrollUp => some (focusPreviousContext { app with panelFocus := .drawer })
        | .scrollDown => some (focusNextContext { app with panelFocus := .drawer })
        | .press =>
            if mouse.button != .left then none
            else match contextFormulaAtRow app rightWidth mouse.row with
            | some id => some (focusContextItem { app with panelFocus := .drawer } id)
            | none => match contextHitAtRow app rightWidth mouse.row with
              | none => none
              | some (index, header) =>
                  let app := focusContext { app with panelFocus := .drawer } index
                  some (if header then toggleFocusedContext app else app)
        | _ => none

private def reportTestApp : App := {
  entries := [{
    cell := 1
    input := "/check"
    output := "full prover output"
    ok := false
    report := some (Report.error "prover needs attention")
  }]
}

#guard match handleMouse reportTestApp { columns := 110, rows := 28 }
    { button := .left, action := .press, column := 5, row := 3 } with
  | some app => app.entries.head?.map (·.reportExpanded) == some true
  | none => false

#guard match handleMouse ({ reportTestApp with
    runOpen := true
    runRows := #[{ name := "metis", status := .result .theorem }] } : App)
    { columns := 110, rows := 28 }
    { button := .left, action := .press, column := 5, row := 3 } with
  | some app => app.entries.head?.map (·.reportExpanded) == some true
  | none => false

#guard match handleMouse ({ stateOpen := true } : App) { columns := 110, rows := 28 }
    { button := .none, action := .scrollUp, column := 1, row := 3 } with
  | some app => app.transcriptScroll == 3 && app.panelFocus == .main
  | none => false

private def interactive (initial : App) : IO Unit := do
  withAlternateScreen do
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
      keymap := some appKeymap
      handleMouse := handleMouse
      getState := fun app => app.repl
      setState := fun app repl => { (clearSelection app) with repl }
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
    let timing := entry.elapsedMs.map (fun milliseconds =>
      s!" ({formatElapsed milliseconds})") |>.getD ""
    IO.println s!"    {(if entry.ok then "=" else "!")} {entry.output}{timing}"

private def usage : String :=
  "oatp repl — interactive theorem-proving workbench\n\n" ++
  "usage:\n  lake exe oatp repl\n  lake exe oatp repl --script FILE\n\n" ++
  "examples:\n  /load problem.p\n  /to-lean p => p\n  /snapshot\n  " ++
  "/to-tptp\n  " ++
  "/reconstruct implication-intro h exact h\n  /term"

private def scriptExitCode (app : App) : UInt32 :=
  if app.entries.all (·.ok) then 0 else 1

private def initialApp (runtime : OATP.Lean.Repl.Runtime) : IO App := do
  let (prefs, warning) ← OATP.Config.load
  let runProgress ← IO.mkRef (#[] : Array RunRow)
  let scheme := themeByName prefs.theme |>.getD aurora
  let warning := warning.orElse fun _ =>
    if themeByName prefs.theme |>.isSome then none
    else some s!"unknown configured theme `{prefs.theme}`; using {defaultThemeName}"
  pure {
    leanRuntime := some runtime
    stateOpen := true
    panelFocus := .main
    runProgress := some runProgress
    theme := scheme
    themeName := if themeByName prefs.theme |>.isSome then prefs.theme else defaultThemeName
    theory := prefs.theory
    strategy := OATP.RunStrategy.ofString prefs.strategy |>.getD .all
    defaultProver := OATP.ProverReference.fromPersisted prefs.defaultProver
    enabledProvers := prefs.enabledProvers.toList.filterMap
      OATP.ProverReference.fromPersisted |>.toArray
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
