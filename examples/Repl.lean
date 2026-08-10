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

private def numericOption (name : String) (args : List String) (default : Nat) : Nat :=
  match args with
  | flag :: value :: _ => if flag == name then value.toNat?.getD default else default
  | _ => default

private def runReferences (app : App) (references : List String) : IO (String × Bool) := do
  match currentProblem app with
  | none => pure ("no current problem; add a TPTP conjecture or translate a Lean goal", false)
  | some problem =>
      let timeout := numericOption "--timeout" references 30
      let maxOutput := numericOption "--max-output" references (4 * 1024 * 1024)
      let references := references.filter (fun reference =>
        !reference.startsWith "--" && reference != "" && reference.toNat?.isNone)
      let references ← if references.isEmpty then
        pure (← OATP.Runtime.installedProvers).toList
      else pure references
      let localReferences := references.filter (!·.startsWith "online-")
      let onlineReferences := references.filter (·.startsWith "online-")
      let limits : Limits := { wallSeconds := timeout, maxOutputBytes := maxOutput }
      let mut attempts : Array Portfolio.Attempt := #[]
      for reference in localReferences do
        attempts := attempts.push {
          name := reference
          limits
          backend := .local { executable := reference }
        }
      if !onlineReferences.isEmpty then
        let endpoint := SystemOnTPTP.defaultEndpoint
        match ← OATP.Runtime.loadCatalogue "oatp-repl" endpoint .normal with
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
                    timeLimit := timeout
                    maxBodyBytes := maxOutput
                  }
                }
      if attempts.isEmpty then
        pure ("no prover selected or installed", false)
      else
        let results ← Portfolio.run problem attempts
        let rendered := results.toList.map artifactText
        pure (String.intercalate "\n" (rendered.map Prod.fst), rendered.any Prod.snd)

private def systemsText (online : Bool) : IO String := do
  let installed ← OATP.Runtime.installedProvers
  let lines := if installed.isEmpty then ["LOCAL: none"] else
      ["LOCAL:"] ++ installed.toList.map (fun prover => "  " ++ prover)
  if !online then
    pure <| String.intercalate "\n" lines ++ "\nONLINE: opt-in with /systems --online"
  else
    match ← OATP.Runtime.loadCatalogue "oatp-repl" SystemOnTPTP.defaultEndpoint .normal with
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

private def parseStep (source : String) : Except String OATP.Proof.Step :=
  match words source with
  | ["true-intro"] => pure .trueIntro
  | ["exact", name] => pure (.exact (Name.mkSimple name))
  | ["and-left", name] => pure (.andLeft (Name.mkSimple name))
  | ["and-right", name] => pure (.andRight (Name.mkSimple name))
  | ["implication-intro", name, "exact", body] =>
      pure (.implicationIntro (Name.mkSimple name) (.exact (Name.mkSimple body)))
  | _ => .error "proof steps: true-intro | exact NAME | implication-intro NAME exact NAME"

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
  if line.startsWith "/goal " then
    return some (← submitGoal app cell input (line.drop "/goal ".length).trimAscii.toString)
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
  if let some result ← submitLeanCommand app cell input then
    return result
  if input == "/state" then
    let session := OATP.Repl.note app.session input "state drawer toggled"
    return { (appendEntry { app with session, stateOpen := !app.stateOpen } cell input
        "state drawer toggled" true) with }
  if input == "/systems" || input == "/systems --online" then
    let output ← systemsText (input.endsWith "--online")
    return note app cell input output true
  if input == "/doctor" then
    return note app cell input (← doctorText) true
  if input == "/run" || input.startsWith "/run " then
    let args := words input |>.drop 1
    let (output, ok) ← runReferences app args
    return note app cell input output ok
  if input == "/local" || input.startsWith "/local " then
    let args := words input |>.drop 1
    let (output, ok) ← runReferences app args
    return note app cell input output ok
  if input == "/online" || input.startsWith "/online " then
    let args := words input |>.drop 1
    let args := args.map (fun reference => if reference.startsWith "online-" then reference else "online-" ++ reference)
    let (output, ok) ← runReferences app args
    return note app cell input output ok
  match OATP.Repl.apply app.session input with
  | .ok session =>
      let output := lastHistory session
      pure <| appendEntry { app with session } cell input output true
  | .error message => pure (note app cell input message false)

private def commandNames : List String :=
  ["/help", "/history", "/state", "/clear", "/reset", "/axiom", "/conjecture", "/parse",
   "/goal", "/snapshot", "/to-tptp", "/reconstruct", "/term", "/run", "/local", "/online",
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
  "examples:\n  /conjecture goal p => p\n  /goal p => p\n  /to-tptp\n  /reconstruct implication-intro h exact h\n  /term"

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
             "/to-tptp", "/reconstruct implication-intro h exact h", "/term"]
          else [String.intercalate " " args]
        staticOutput (← runScript initial lines)
