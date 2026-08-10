/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP
import OATP.ReplView
import TermColor.Repl.Terminal
import TermColor.Terminal

/-!
# oatp-repl

The interactive shell combines the pure TPTP session model with OATP's existing process, portfolio,
catalogue, and Lean proof boundaries. Static script mode keeps the same command path testable in CI.
-/

open OATP
open OATP.ReplView
open Lean
open TermColor
open TermColor.Repl
open TermColor.Terminal
open TermColor.Widgets

private def words (line : String) : List String :=
  line.splitOn " " |>.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def appendEntry (app : App) (cell : Nat) (input output : String) (ok : Bool) : App :=
  { app with
    entries := { cell, input, output, ok } :: app.entries
    repl := {} 
    status := if ok then "ready" else "error" }

private def note (app : App) (cell : Nat) (input output : String) (ok : Bool) : App :=
  appendEntry { app with session := OATP.Repl.note app.session input output } cell input output ok

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

private def runRequest (app : App) (request : OATP.Repl.RunRequest) : IO (String × Bool) := do
  match currentProblem app with
  | none => pure ("no current problem; add a TPTP conjecture or translate a Lean goal", false)
  | some problem =>
      let references ← if request.references.isEmpty then
        pure (← OATP.Runtime.installedProvers).toList
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
    unless ← TermColor.Repl.Terminal.Cancellation.sleep cancellation 1 do
      return app
    match backendRequest input with
    | .error message =>
        pure { app with jobResult := some { cell := app.session.nextCell, input, output := message, ok := false } }
    | .ok request =>
        -- ponytail: portfolio execution has no process-cancellation seam yet; keep the UI
        -- responsive and add cancellation at Portfolio once process ownership is exposed.
        let (output, ok) ← runRequest app request
        pure { app with jobResult := some { cell := app.session.nextCell, input, output, ok } }
  finish := fun current completed =>
    match completed.jobResult with
    | some result =>
        note { current with busy := false, jobResult := none }
          result.cell result.input result.output result.ok
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
  pure <| String.intercalate "\n" [
    "transport: " ++ if transports.isEmpty then "unavailable" else String.intercalate ", " transports.toList,
    "local: " ++ if installed.isEmpty then "none" else String.intercalate ", " installed.toList,
    "online: " ++ if transports.isEmpty then "unavailable" else "ready"]

private partial def parseStepTokens : List String → Except String (OATP.Proof.Step × List String)
  | "true-intro" :: rest => pure (.trueIntro, rest)
  | "exact" :: name :: rest => pure (.exact (Name.mkSimple name), rest)
  | "and-left" :: name :: rest => pure (.andLeft (Name.mkSimple name), rest)
  | "and-right" :: name :: rest => pure (.andRight (Name.mkSimple name), rest)
  | "and-intro" :: rest => do
      let (left, rest) ← parseStepTokens rest
      let (right, rest) ← parseStepTokens rest
      pure (.andIntro left right, rest)
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
  | .error message => pure (note app cell input message false)
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

private def submit (app : App) (input : String) : IO App := do
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
          return appendEntry { app with session } cell input (lastHistory session) true
      | .error message => return note app cell input message false
    catch error =>
      return note app cell input s!"could not read `{path}`: {error}" false
  if let some result ← submitLeanCommand app cell input then
    return result
  if input == "/state" then
    let session := OATP.Repl.note app.session input "state drawer toggled"
    return { (appendEntry { app with session, stateOpen := !app.stateOpen } cell input
        "state drawer toggled" true) with }
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
      pure <| appendEntry { app with session } cell input output true
  | .error message => pure (note app cell input message false)

private def commandNames : List String :=
  ["/help", "/history", "/state", "/clear", "/reset", "/load", "/axiom", "/conjecture", "/parse",
   "/goal", "/to-lean", "/translate-to-lean", "/snapshot", "/to-tptp", "/reconstruct", "/term",
   "/run", "/local", "/online",
   "/systems", "/doctor", "/quit", "/exit"]

private def complete (_app : App) (input : TextInputState) : IO (List Completion) := do
  let candidates := commandNames.filter (·.startsWith input.value)
  pure (candidates.map (fun replacement => { replacement }))

private def interactive (initial : App) : IO Unit := do
  clearScreen
  TermColor.Repl.Terminal.run {
    initial
    inputConfig := inputConfig
    multiline := some multilineConfig
    fallbackSize := fallbackSize
    tickMs := 16
    view := fun app size => screen app size
    complete := complete
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
    unless line.trimAscii.toString.isEmpty do
      app ← submit app line
  pure app

private def staticOutput (app : App) : IO Unit := do
  for entry in app.entries.reverse do
    IO.println s!"[{entry.cell}] › {entry.input}"
    IO.println s!"    {(if entry.ok then "=" else "!")} {entry.output}"

private def usage : String :=
  "oatp-repl — interactive TPTP/Lean ATP workbench\n\n" ++
  "usage:\n  lake exe oatp-repl\n  lake exe oatp-repl --script FILE\n\n" ++
  "examples:\n  /load problem.p\n  /to-lean p => p\n  /snapshot\n  /to-tptp\n  /reconstruct implication-intro h exact h\n  /term"

def main (args : List String) : IO Unit := do
  if args == ["--help"] || args == ["-h"] then
    IO.println usage
    return
  let runtime ← OATP.Lean.Repl.create
  let initial : App := { leanRuntime := some runtime }
  let script? ← match args with
    | ["--script", path] => pure (some (← IO.FS.readFile path))
    | _ => pure none
  let interactiveTerminal ← do
    pure ((← stdoutIsTty) && (← stdinIsTty) && (← stdoutSupportsControl))
  let blocked := (← IO.getEnv "CI").isSome || (← IO.getEnv "OATP_NONINTERACTIVE").isSome
  match script? with
  | some source =>
      let result ← runScript initial (source.splitOn "\n")
      staticOutput result
  | none =>
      if interactiveTerminal && !blocked && args.isEmpty then
        interactive initial
      else
        let lines := if args.isEmpty then
          ["/conjecture goal p => p", "/state", "/goal p => p", "/snapshot",
             "/to-lean p => p", "/to-tptp", "/reconstruct implication-intro h exact h", "/term"]
          else [String.intercalate " " args]
        staticOutput (← runScript initial lines)
