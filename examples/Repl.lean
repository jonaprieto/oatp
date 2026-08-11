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
    transcriptScroll := 0
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

private def savePreferences (before after : App) : IO App := do
  if preferences before == preferences after then
    pure after
  else
    match ← OATP.Config.save (preferences after) with
    | none => pure after
    | some message => pure { after with statusNotice := some message }

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
  | none => pure ("no current problem; try `/parse fof(goal, conjecture, p => p).` or `/load FILE`", false)
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
  match OATP.Repl.parseCommandSpec input with
  | .ok command => match command with
      | .run _ | .local _ | .online _ => true
      | _ => false
  | .error _ => false

private def backendRequest (input : String) : Except String OATP.Repl.RunRequest :=
  match OATP.Repl.parseCommandSpec input with
  | .ok (.run request) => pure request
  | .ok (.local request) => pure {
      references := [request.executable]
      timeout := request.timeout
      maxOutput := request.maxOutput
      arguments := request.arguments }
  | .ok (.online request) => pure {
      references := [if request.system.startsWith "online-" then request.system
        else "online-" ++ request.system]
      endpoint := request.endpoint
      timeout := request.timeout
      maxOutput := request.maxOutput }
  | .ok _ => .error "not a prover command"
  | .error message => .error message

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

private def submitTheoryFormula (app : App) (cell : Nat) (input : String)
    (command : OATP.Repl.Command) : IO (Option App) := do
  match command with
  | .axiom name formula | .conjecture name formula =>
      let role := match command with | .axiom _ _ => "axiom" | _ => "conjecture"
      let source := s!"{app.theory}({name}, {role}, {formula})."
      match OATP.Repl.parseSource app.session input source with
      | .ok session => pure (some (note { app with session } cell input
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
        let output := String.intercalate "\n" snapshot.context.toList ++ "\n⊢ " ++ snapshot.target
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
      let output := lastHistory session
      let app := if changesContext input then clearDerived { app with session }
        else { app with session }
      let app := if input == "/reset" then { app with entries := [] } else app
      let entryCell := if input == "/reset" then 1 else cell
      pure <| appendEntry app entryCell input output true
  | .error message => pure (note app cell input message false)

private def submitCommand (app : App) (cell : Nat) (input : String)
    (command : OATP.Repl.Command) : IO App := do
  match command with
  | .quit => pure { app with running := false }
  | .load path => do
      try
        let source ← IO.FS.readFile path
        match OATP.Repl.parseSource app.session input source with
        | .ok session => pure (appendEntry (clearDerived { app with session }) cell input
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
      let session := OATP.Repl.note app.session input "state drawer toggled"
      let updated := { app with
        session := session
        stateOpen := !app.stateOpen
        historyOpen := false
        proversOpen := false }
      pure (appendEntry updated cell input "state drawer toggled" true)
  | OATP.Repl.Command.stateTarget stateName =>
      match openContextTarget { app with historyOpen := false, proversOpen := false } stateName with
      | some focused => pure (note focused cell input s!"state: {stateName}" true)
      | none => pure (note app cell input
          s!"unknown state target `{stateName}`; try: {String.intercalate ", " contextTargetNames}" false)
  | .history =>
      let session := OATP.Repl.note app.session input "history drawer toggled"
      pure (appendEntry { app with session, historyOpen := !app.historyOpen, stateOpen := false }
        cell input "history drawer toggled" true)
  | .theme none => pure (note app cell input
      s!"theme: {app.themeName}; available: {themeNames}" true)
  | .theme (some name) =>
      match themeByName name with
      | some scheme => pure (note { app with theme := scheme, themeName := name } cell input
          s!"theme changed to {name}" true)
      | none => pure (note app cell input s!"unknown theme `{name}`; try: {themeNames}" false)
  | .theory none => pure (note app cell input "theory: current; available: fof, cnf, tff" true)
  | .theory (some requested) =>
      let requested := requested.toLower
      let theory := if requested == "tf1" then "tff" else requested
      if theory == "fof" || theory == "cnf" || theory == "tff" then
        pure (note { app with theory } cell input s!"theory set to {theory}" true)
      else pure (note app cell input "unknown theory; use fof, cnf, or tff" false)
  | .prover none => pure (note app cell input
      s!"default prover: {if app.defaultProver.isEmpty then "auto" else app.defaultProver}" true)
  | .prover (some name) => do
      let candidates ← OATP.Runtime.localProverCandidates
      let found := candidates.filter (fuzzy name)
      match found.toList with
      | [resolved] => pure (note { app with defaultProver := resolved } cell input
          s!"default prover set to {resolved}" true)
      | [] => pure (note app cell input s!"unknown prover `{name}`" false)
      | _ => pure (note app cell input (s!"ambiguous prover `{name}`: " ++
          String.intercalate ", " found.toList) false)
  | .provers => do
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
  | .run request => do
      let (output, ok) ← runRequest app request
      pure (note app cell input output ok)
  | .local request => do
      let run : OATP.Repl.RunRequest := {
        references := [request.executable]
        timeout := request.timeout
        maxOutput := request.maxOutput
        arguments := request.arguments }
      let (output, ok) ← runRequest app run
      pure (note app cell input output ok)
  | .online request => do
      let run : OATP.Repl.RunRequest := {
        references := [if request.system.startsWith "online-" then request.system
          else "online-" ++ request.system]
        endpoint := request.endpoint
        timeout := request.timeout
        maxOutput := request.maxOutput }
      let (output, ok) ← runRequest app run
      pure (note app cell input output ok)
  | _ => applyPureCommand app cell input

private def submitCore (app : App) (input : String) : IO App := do
  let input := input.trimAscii.toString
  let cell := app.session.nextCell
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
  | "TOPIC" => pure ["cnf", "fof", "tff", "lean", "run", "context", "grammar", "roles"]
  | "FORMAT" => pure ["cnf", "fof", "tff"]
  | "TARGET" => pure (contextTargetNames ++ ["all"])
  | "THEORY" => pure ["fof", "cnf", "tff", "tf1"]
  | "THEME" => pure (themes.map Prod.fst)
  | "STEP" => pure ["true-intro", "exact", "and-left", "and-right", "and-intro",
      "implication-intro"]
  | "PROVER" | "SYSTEM" =>
      pure (← OATP.Runtime.localProverCandidates).toList
  | _ => pure []

private def complete (_app : App) (input : TextInputState) : IO (List Completion) :=
  completeCommandWith OATP.Repl.commandSpec commandValues input

private inductive AppKeyAction
  | closeProvers
  | proverNext
  | proverPrevious
  | toggleProver
  | closeState
  | contextNext
  | contextPrevious
  | toggleContext
  | expandContext
  | collapseContext
  | closeHistory
  | transcriptPageUp
  | transcriptPageDown

private def appKeymap : TermColor.Repl.Terminal.AppKeymap App where
  Action := AppKeyAction
  keymap := { bindings :=
    [ { key := .char 'H', action := .closeProvers, context := some "provers" }
    , { key := .escape, action := .closeProvers, context := some "provers" }
    , { key := .char 'J', action := .proverNext, context := some "provers" }
    , { key := .down, action := .proverNext, context := some "provers" }
    , { key := .char 'K', action := .proverPrevious, context := some "provers" }
    , { key := .up, action := .proverPrevious, context := some "provers" }
    , { key := .enter, action := .toggleProver, context := some "provers" }
    , { key := .char ' ', action := .toggleProver, context := some "provers" }
    , { key := .char 'H', action := .closeState, context := some "state" }
    , { key := .char 'J', action := .contextNext, context := some "state" }
    , { key := .down, action := .contextNext, context := some "state" }
    , { key := .char 'K', action := .contextPrevious, context := some "state" }
    , { key := .up, action := .contextPrevious, context := some "state" }
    , { key := .enter, action := .toggleContext, context := some "state" }
    , { key := .char ' ', action := .toggleContext, context := some "state" }
    , { key := .right, action := .expandContext, context := some "state" }
    , { key := .left, action := .collapseContext, context := some "state" }
    , { key := .escape, action := .collapseContext, context := some "state" }
    , { key := .char 'H', action := .closeHistory, context := some "history" }
    , { key := .pageUp, action := .transcriptPageUp, context := some "default" }
    , { key := .pageDown, action := .transcriptPageDown, context := some "default" } ] }
  contexts := fun app =>
    if app.proversOpen then ["provers"]
    else if app.stateOpen then ["state"]
    else if app.historyOpen then ["history"]
    else ["default"]
  handle := fun app action => some (clearSelection (match action with
    | .closeProvers => { app with proversOpen := false }
    | .proverNext => focusNextProver app
    | .proverPrevious => focusPreviousProver app
    | .toggleProver => toggleFocusedProver app
    | .closeState => { app with stateOpen := false }
    | .contextNext => focusNextContext app
    | .contextPrevious => focusPreviousContext app
    | .toggleContext => toggleFocusedContext app
    | .expandContext => expandFocusedContext app
    | .collapseContext => collapseFocusedContext app
    | .closeHistory => { app with historyOpen := false }
    | .transcriptPageUp => scrollTranscriptPageUp app
    | .transcriptPageDown => scrollTranscriptPageDown app))

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
    | .scrollUp => some (clearSelection (scrollTranscriptUp app))
    | .scrollDown => some (clearSelection (scrollTranscriptDown app))
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
    stateOpen := true
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
