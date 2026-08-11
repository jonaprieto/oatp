/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP
import OATP.TPTP
import OATP.SystemOnTPTP
import OATP.Term
import TermColor.Repl.Command

open OATP OATP.TPTP

#guard SZSStatus.toString .theorem == "Theorem"
#guard SZSStatus.ofOutput "# SZS status Theorem for fixture\n" == some .theorem
#guard SZSStatus.ofOutput "% SZS status Timeout for fixture\n" == some .timeout
#guard SZSStatus.ofOutput
    "% RESULT: fixture - Vampire says Timeout - CPU = 2 WC = 2\n" == some .timeout
#guard (parseStatement "fof(goal, conjecture, p)." |>.isOk)
#guard (parseStatement "cnf(c1, axiom, p | ~q)." |>.isOk)
#guard match _root_.TPTP.TFF.parseFormulaString "#[X:$i] : p(X)" with
  | .ok formula =>
      formula.render.startsWith "# [X: $i]" &&
        (_root_.TPTP.TFF.validateFormula {} formula).isOk
  | .error _ => false
#guard match _root_.TPTP.TFF.parseTypeDeclarationString
    "identity: !>[A:$tType] : (A > A)" with
  | .ok declaration =>
      (_root_.TPTP.TFF.validateDeclaration {} declaration).isOk
  | .error _ => false
#guard match parseStatement "fof(goal, conjecture, p(f(a,b)))." with
  | .ok statement => statement.formula == "p(f(a,b))" && statement.annotations.isNone
  | .error _ => false
#guard match parseStatement "fof(goal, conjecture, p('x)'), status(thm, theorem))." with
  | .ok statement => statement.formula == "p('x)')" &&
      statement.annotations == some "status(thm, theorem)"
  | .error _ => false
#guard match parseStatement "not-tptp" with
  | .error _ => true
  | .ok _ => false
#guard match parseStatement "fof(goal, conjecture, p). trailing" with
  | .error _ => true
  | .ok _ => false
#guard match OATP.SystemOnTPTP.parseResponse
    { systemLabel := "vampire" }
    { name := "goal", source := "fof(goal, conjecture, p)." }
    { statusCode := 200, body := "% SZS status Theorem for goal\n" } with
  | .ok artifact => artifact.status == .theorem && artifact.prover.name == "vampire" &&
      artifact.problemName == some "goal"
  | .error _ => false
#guard match OATP.SystemOnTPTP.parseResponse
    { systemLabel := "vampire" }
    { name := "goal", source := "" }
    { statusCode := 500, body := "server error" } with
  | .error (.httpStatus 500) => true
  | _ => false
#guard (OATP.SystemOnTPTP.Catalogue.parse
    ("<input NAME=\"System___Vampire---5.0.1\" " ++
      "value=\"Vampire---5.0.1\">\n<input name=\"System___E---3.5.1\">")).size == 2
#guard match OATP.SystemOnTPTP.Catalogue.parse
    ("<input NAME=\"System___Vampire---5.0.1\" value=\"Vampire---5.0.1\">" ++
      "\n<input name=\"System___E---3.5.1\">") with
  | systems => (OATP.SystemOnTPTP.Catalogue.resolve systems "online-vampire").isSome &&
      (OATP.SystemOnTPTP.Catalogue.resolve systems "online-E---3.5.1").isSome
#guard match OATP.Runtime.resolveOnline "oatp" #[{ id := "Vampire---5.0.1" }]
    ["online-vampire"] with
  | .ok #[system] => system.id == "Vampire---5.0.1"
  | _ => false
#guard match (OATP.SystemOnTPTP.Catalogue.parse
    ("<input NAME=\"System___Vampire---5.0.1\">\n" ++
      "<input name=\"Command___Vampire---5.0.1\" value=\"run_vampire %s %d THM\">\n" ++
      "<input name=\"TimeLimit___Vampire---5.0.1\" value=\"12\">")).toList with
  | [system] => system.command == "run_vampire %s %d THM" && system.timeLimit == 12
  | _ => false
#guard (OATP.SystemOnTPTP.fields
    { systemLabel := "one", systemLabels := #["one", "two"] }
    { name := "goal", source := "fof(goal, conjecture, p)." }).size == 16
#guard OATP.Http.Form.encodeComponent "a b&c" == "a%20b%26c"
#guard OATP.Http.Form.encodeUrlEncoded #[
  { name := "x", value := "a b" },
  { name := "y", value := "✓" }
] == "x=a%20b&y=%E2%9C%93"
#guard match OATP.Http.Form.encodeMultipart "oatp-boundary" #[
    { name := "problem", value := "fof(goal, conjecture, p)." }
  ] with
  | .ok body => body.startsWith "--oatp-boundary\r\n"
  | .error _ => false
#guard match OATP.Http.Form.encodeMultipart "bad\r\n" #[] with
  | .error _ => true
  | .ok _ => false
#guard match OATP.Http.Form.encodeMultipart "boundary" #[
    { name := "problem", value := "contains-boundary" }
  ] with
  | .error _ => true
  | .ok _ => false
#guard OATP.Term.renderPlain #[.goal {
  title := "demo"
  context := #["h : p"]
  target := "p"
}] == "goal: demo\n  h : p\n⊢ p"
#guard match OATP.Repl.parseInput "/conjecture goal p" with
  | .command (.conjecture "goal" "p") => true
  | _ => false
#guard match OATP.Repl.parseInput "/help cnf" with
  | .command (.help (some "cnf")) => true
  | _ => false
#guard match OATP.Repl.parseInput "/state goal" with
  | .command (.stateTarget "goal") => true
  | _ => false
#guard match OATP.Repl.parseInput "/grammar cnf" with
  | .command (.grammar "cnf") => true
  | _ => false
#guard match OATP.Repl.parseInput "/roles tff" with
  | .command (.roles (some "tff")) => true
  | _ => false
#guard match OATP.Repl.apply {} "/help cnf" with
  | .ok session =>
      let help := (session.history.toList.getLast?.map (·.result)).getD ""
      help.contains "CNF" && help.contains "cnf(c1, axiom" && help.contains "implicitly universal"
  | .error _ => false
#guard match OATP.Repl.parseRunRequest
    ["--prover", "eprover", "--timeout", "7", "--max-output", "99", "--no-cache",
      "--", "--foo"] with
  | .ok request => request.references == [OATP.Repl.ProverReference.fromLocal "eprover"] &&
      request.timeout == 7 &&
      request.maxOutput == 99 && request.noCache && request.arguments == ["--foo"]
  | .error _ => false
#guard match OATP.Repl.parseRunRequest ["--all"] with
  | .ok request => request.all
  | .error _ => false
#guard OATP.Argus.ResourceOptions.spec.toMeta.flags.map (·.long) == ["timeout", "max-output"]
#guard OATP.Argus.CatalogueOptions.spec.toMeta.flags.map (·.long) ==
  ["endpoint", "refresh", "no-cache"]
#guard match OATP.Repl.parseRunRequest ["--timeout", "2m", "--max-output", "2Mi"] with
  | .ok request => request.timeout == 120 && request.maxOutput == 2 * 1024 * 1024
  | .error _ => false
#guard match OATP.Repl.parseCommandSpec "/run --all -- --foo" with
  | .ok (.run request) => request.all && request.arguments == ["--foo"]
  | _ => false
#guard match OATP.Repl.parseLocalRequest
    ["--executable", "eprover", "--timeout", "4", "--", "--foo"] with
  | .ok request => request.executable == "eprover" && request.timeout == 4 &&
      request.arguments == ["--foo"]
  | .error _ => false
#guard match OATP.Repl.parseOnlineRequest ["--system", "online-vampire", "--timeout", "5"] with
  | .ok request => request.system == "online-vampire" && request.timeout == 5
  | .error _ => false
#guard match OATP.Repl.parseSource {} "fof(goal, conjecture, p(X))."
    "fof(goal, conjecture, p(X))." with
  | .ok session =>
      session.formulas.size == 1 &&
      session.symbols.any (fun symbol => symbol.kind == .predicate && symbol.name == "p") &&
      session.symbols.any (fun symbol => symbol.kind == .variable && symbol.name == "X")
  | .error _ => false
#guard (OATP.ReplView.screen {} { columns := 100, rows := 24 }).plainText.contains "OATP REPL"
#guard (OATP.ReplView.screen { stateOpen := false }
    { columns := 100, rows := 24 }).plainText.contains
  "ATP ORCHESTRATION"
#guard (OATP.ReplView.screen { stateOpen := false }
    { columns := 100, rows := 24 }).plainText.contains
  "◆───┼───◆"
#guard (OATP.ReplView.screen { stateOpen := false }
    { columns := 100, rows := 24 }).plainText.contains
  "create a Lean goal"
#guard (OATP.ReplView.screen {} { columns := 100, rows := 24 }).plainText.contains "/help"
#guard ({} : OATP.ReplView.App).stateOpen && ({} : OATP.ReplView.App).panelFocus == .main
def completionApp : OATP.ReplView.App :=
  { repl := { input := { value := "/st", cursor := 3 }
              completion := some { candidates := #[{ replacement := "/state" }] } } }
#guard (OATP.ReplView.screen completionApp { columns := 100, rows := 24 }).plainText.contains
  "/state"
#guard OATP.ReplView.formatElapsed 1_500 == "1.5 s"
private def timedEntry : OATP.ReplView.TranscriptEntry :=
  { cell := 1
    input := "/to-lean p => p"
    output := "goal created"
    elapsedMs := some 12 }
#guard (OATP.ReplView.screen
    { entries := [timedEntry] }
    { columns := 100, rows := 24 }).plainText.contains "(12 ms)"
private def plainEntry : OATP.ReplView.TranscriptEntry :=
  { cell := 1
    input := "/to-lean p => p"
    output := "goal created" }
#guard (OATP.ReplView.screen { stateOpen := true } { columns := 100, rows := 24 }).plainText
    |>.splitOn "\n" |>.all (·.length ≤ 100)
#guard (OATP.ReplView.screen { stateOpen := true } { columns := 80, rows := 24 }).plainText
    |>.splitOn "\n" |>.all (·.length ≤ 80)
#guard (OATP.ReplView.screen
    { entries := [plainEntry] }
    { columns := 100, rows := 24 }).segments.any
      (fun segment => segment.text == "=>" && !segment.style.settings.isEmpty)
#guard (OATP.ReplView.screen
    { transcriptScroll := 10
      entries := [
        ({ cell := 3, input := "/help tff", output := "latest" } : OATP.ReplView.TranscriptEntry),
        ({ cell := 2, input := "/help fof", output := "middle" } : OATP.ReplView.TranscriptEntry),
        ({ cell := 1, input := "/help cnf", output := "old" } : OATP.ReplView.TranscriptEntry)] }
    { columns := 100, rows := 10 }).plainText.contains "old"
#guard (OATP.ReplView.screen
    { stateOpen := true, translation := some "fof(goal, conjecture, p)." }
    { columns := 110, rows := 24 }).plainText.contains "LEAN → TPTP"
#guard
  (OATP.ReplView.screen { stateOpen := true } { columns := 110, rows := 24 }).plainText.contains
  "▸ FORMULAS (0)"
#guard
  (OATP.ReplView.screen { stateOpen := true } { columns := 110, rows := 24 }).plainText.contains
  "state • inactive • Ctrl-] focus"
#guard
  (OATP.ReplView.screen { stateOpen := true } { columns := 110, rows := 24 }).plainText.contains
  "input • Ctrl-]"
#guard (OATP.ReplView.screen
    { runOpen := true, panelFocus := .drawer,
      runRows := #[({ name := "eprover", status := .running } : OATP.ReplView.RunRow)] }
    { columns := 110, rows := 24 }).plainText.contains "running"
#guard match OATP.Repl.apply {} "/help context" with
  | .ok session =>
      (session.history.toList.getLast?.map (·.result)).getD "" |>.contains "Ctrl-]"
  | .error _ => false
#guard match OATP.Repl.apply {} "/help /to-lean" with
  | .ok session =>
      let help := (session.history.toList.getLast?.map (·.result)).getD ""
      help.contains "/to-lean" && help.contains "1. /goal"
  | .error _ => false
#guard OATP.Repl.commandNames.all (fun command =>
  !(OATP.Repl.helpFor (some command)).contains "unknown help topic")
#guard match OATP.Repl.apply {} "/help local" with
  | .ok session =>
      (session.history.toList.getLast?.map (·.result)).getD "" |>.contains "/local"
  | .error _ => false
#guard match OATP.Repl.parseCommandSpec "/local" with
  | .error message => message.contains "missing required argument" && message.contains "/help local"
  | .ok _ => false
#guard match OATP.Repl.parseCommandSpec "/run" with
  | .ok (.run request) => request.references.isEmpty && !request.all
  | _ => false
#guard match OATP.Repl.parseCommandSpec "local" with
  | .error message => message.contains "commands start with"
  | .ok _ => false
#guard match OATP.Repl.parseCommandSpec "/help\tcnf" with
  | .ok (.help (some "cnf")) => true
  | _ => false
#guard OATP.Runtime.defaultLocalProver #[] == none
#guard OATP.Runtime.defaultLocalProver #["eprover", "vampire"] == some "eprover"
#guard (OATP.ReplView.toggleFocusedContext {}).contextExpanded.getD 0 false
#guard OATP.ReplView.contextTargetOfString "form" == some 0
#guard OATP.ReplView.contextTargetOfString "tptp" == some 4
#guard (OATP.ReplView.openContextTarget {} "goal").map
    (fun app => app.stateOpen && app.contextFocus == 3 && app.contextExpanded.getD 3 false) ==
      some true
#guard OATP.ReplView.themeByName "dracula" |>.isSome
#guard "to-lean" ∈ OATP.Repl.commandNames
#guard OATP.Repl.ProverReference.fromPersisted "local:online-local" ==
  some (OATP.Repl.ProverReference.fromLocal "online-local")
#guard OATP.Repl.ProverReference.fromPersisted "online:vampire" ==
  some (OATP.Repl.ProverReference.fromOnline "vampire")
#guard OATP.ProverReference.fromPersisted "local:" == none
#guard OATP.ProverReference.fromPersisted "online:" == none
#guard (OATP.ReplView.focusNextContext {}).contextFocus == 1
#guard (OATP.ReplView.focusPreviousContext {}).contextFocus == 5
#guard OATP.ReplView.contextHitAtRow {} 40 2 == some (0, true)
#guard (OATP.ReplView.screen
    { historyOpen := true
      session := { history := #[{ cell := 1, input := "/help", result := "commands" }] } }
    { columns := 100, rows := 24 }).plainText.contains "history • active"
#guard (OATP.ReplView.screen { historyOpen := true } { columns := 100, rows := 24 }).height == 24
#guard (OATP.ReplView.screen
    { entries := [{ cell := 1, input := "/snapshot", output := "first\nsecond" }] }
    { columns := 100, rows := 24 }).plainText.contains "second"
#guard match OATP.Repl.apply {} "/help" with
  | .ok session =>
      let help := (session.history.toList.getLast?.map (·.result)).getD ""
      help.contains "/help [<TOPIC>]" && help.contains "/goal <FORMULA> [<FORMULA>...]" &&
        help.contains "/axiom <NAME> <FORMULA> [<FORMULA>...]" && help.contains "grammar"
  | .error _ => false
#guard (OATP.ReplView.clearSelection
    { selectionStart := some (1, 2), selectionEnd := some (3, 4) }).selectionStart.isNone
def main : IO UInt32 := do
  let commandCompletions ← TermColor.Repl.completeCommand
    OATP.Repl.commandSpec { value := "/st", cursor := 3 }
  if !(commandCompletions.any (·.replacement == "/state")) then
    throw <| IO.userError "REPL command completion omitted /state"
  let topicCompletions ← TermColor.Repl.completeCommandWith OATP.Repl.commandSpec
    (fun typeName => pure <| if typeName == "TOPIC" then ["cnf", "fof"] else [])
    { value := "/help ", cursor := 6 }
  if !(topicCompletions.any (·.replacement == "cnf")) then
    throw <| IO.userError "REPL topic completion omitted cnf"
  let slashTopicCompletions ← TermColor.Repl.completeCommandWith OATP.Repl.commandSpec
    (fun typeName => pure <| if typeName == "TOPIC" then ["to-lean"] else [])
    { value := "/help /to", cursor := 9 }
  if !(slashTopicCompletions.any (·.replacement == "/to-lean")) then
    throw <| IO.userError "REPL slash-topic completion omitted /to-lean"
  let x := _root_.TPTP.Formula.Term.function "f" #[
    .constant "a", .var "X"
  ]
  let formula := _root_.TPTP.Formula.Expr.forall #["X"]
    (.implies (.atom "p" #[x]) (.atom "q" #[.var "X"]))
  let rendered ← match formula.toTPTP with
    | .ok rendered => pure rendered
    | .error message => throw <| IO.userError message
  if rendered != "![X] : ((p(f(a, X)) => q(X)))" then
    throw <| IO.userError "first-order formula rendering changed"
  match Statement.ofFof "goal" .conjecture formula with
  | .ok statement =>
      if statement.formula != rendered then
        throw <| IO.userError "first-order statement rendering changed"
  | .error message => throw <| IO.userError message
  let invalid := _root_.TPTP.Formula.Expr.atom "Bad" #[]
  match invalid.toTPTP with
  | .error _ => pure ()
  | .ok _ => throw <| IO.userError "invalid TPTP symbol was rendered"
  let unbound := _root_.TPTP.Formula.Expr.atom "p" #[.var "X"]
  match unbound.toTPTP with
  | .error _ => pure ()
  | .ok _ => throw <| IO.userError "unbound TPTP variable was rendered"
  let parsed ← match OATP.TPTP.Syntax.parseFormula
      "![X] : (p(f(X)) => q(X))" with
    | .ok parsed => pure parsed
    | .error message => throw <| IO.userError (message.pretty "![X] : (p(f(X)) => q(X))".toUTF8)
  let parsedRendered ← match parsed.toTPTP with
    | .ok rendered => pure rendered
    | .error message => throw <| IO.userError message
  if parsedRendered != "![X] : ((p(f(X)) => q(X)))" then
    throw <| IO.userError "first-order formula parser round-trip changed"
  let parsedStatement : Statement := {
    kind := .fof
    name := .bare "goal"
    role := .conjecture
    formula := "p(a)"
  }
  match _root_.TPTP.Statement.parseFormula parsedStatement with
  | .ok (.atom "p" #[.constant "a"]) => pure ()
  | _ => throw <| IO.userError "statement semantic formula parse changed"
  match OATP.TPTP.Syntax.parseFormula "p(a) trailing" with
  | .error _ => pure ()
  | .ok _ => throw <| IO.userError "semantic formula parser accepted trailing input"
  let invalidHttp ← OATP.Http.requestWith .curl {
    url := ""
    maxSeconds := 1
  }
  match invalidHttp with
  | .error (.invalidRequest _) => pure ()
  | _ => throw <| IO.userError "invalid HTTP requests were not rejected before transport"
  let oversizedHttp ← OATP.Http.requestWith .curl {
    url := "https://invalid.example"
    body := "123"
    maxRequestBodyBytes := 2
  }
  match oversizedHttp with
  | .error (.requestBodyTooLarge 3 2) => pure ()
  | _ => throw <| IO.userError "HTTP request body limits were not enforced before transport"
  let theoremFixture ← IO.FS.readFile "test/fixtures/system-on-tptp/theorem.txt"
  let theoremArtifact ← match OATP.SystemOnTPTP.parseResponse
      { systemLabel := "vampire" } { name := "fixture", source := "" }
      { statusCode := 200, body := theoremFixture } with
    | .ok artifact => pure artifact
    | .error _ => throw <| IO.userError "theorem fixture did not parse"
  if theoremArtifact.status != .theorem || theoremArtifact.problemName != some "fixture" then
    throw <| IO.userError "theorem fixture metadata changed"
  let timeoutFixture ← IO.FS.readFile "test/fixtures/system-on-tptp/timeout.txt"
  let timeoutArtifact ← match OATP.SystemOnTPTP.parseResponse
      { systemLabel := "vampire" } { name := "fixture", source := "" }
      { statusCode := 200, body := timeoutFixture } with
    | .ok artifact => pure artifact
    | .error _ => throw <| IO.userError "timeout fixture did not parse"
  if timeoutArtifact.status != .timeout then
    throw <| IO.userError "timeout fixture status changed"
  let problem : Problem := { name := "stdin", source := "fof(goal, conjecture, p).\n" }
  let processResult ← OATP.Process.run
    { name := "cat" } problem { wallSeconds := 2 }
    { executable := "cat" }
  match processResult with
  | .ok artifact =>
      if artifact.stdout != problem.source then
        throw <| IO.userError "local process backend did not preserve stdin"
  | .error _ =>
      throw <| IO.userError "local process backend failed to run cat"
  let limited ← OATP.Process.run
    { name := "cat" } problem { wallSeconds := 2, maxOutputBytes := 1 }
    { executable := "cat" }
  match limited with
  | .error (.outputTooLarge actual 1) =>
      if actual ≤ 1 then
        throw <| IO.userError "local process backend reported an invalid output size"
  | _ =>
      throw <| IO.userError "local process backend ignored the output limit"
  let missing ← OATP.Process.run
    { name := "missing" } problem { wallSeconds := 2 }
    { executable := "definitely-not-installed-oatp-prover" }
  match missing with
  | .error (.io _) => pure ()
  | _ => throw <| IO.userError "missing local executable was not reported as an IO error"
  let portfolio ← OATP.Portfolio.run problem #[
    { name := "cat-a", backend := .local { executable := "cat" } },
    { name := "cat-b", backend := .local { executable := "cat" } }
  ]
  if portfolio.size != 2 then
    throw <| IO.userError "portfolio runner did not collect concurrent attempts"
  let largeProblem : Problem := {
    name := "large-stdin"
    source := String.join (List.replicate 200000 "x")
  }
  let largeResult ← OATP.Process.run
    { name := "cat" } largeProblem { wallSeconds := 2 }
    { executable := "cat" }
  match largeResult with
  | .ok artifact =>
      if artifact.stdout != largeProblem.source then
        throw <| IO.userError "local process backend deadlocked on large stdin"
  | .error _ =>
      throw <| IO.userError "local process backend rejected large stdin"
  let missing ← OATP.Process.run
    { name := "missing" } problem { wallSeconds := 2 }
    { executable := "oatp-executable-that-does-not-exist" }
  match missing with
  | .ok artifact =>
      if artifact.status != .error then
        throw <| IO.userError "missing executable was not reported as a process error"
  | .error (.io _) => pure ()
  | .error _ => throw <| IO.userError "local process IO failure was misclassified"
  let leanRuntime ← OATP.Lean.Repl.create
  let (leanRuntime, goal) ← match ← OATP.Lean.Repl.goalFromFormula leanRuntime "p => p" with
    | .ok value => pure value
    | .error message => throw <| IO.userError s!"Lean REPL goal creation failed: {message}"
  let (_, snapshot) ← OATP.Lean.Repl.snapshot leanRuntime goal
  if snapshot.target.isEmpty || snapshot.context.isEmpty then
    throw <| IO.userError "Lean REPL snapshot omitted target or local atom"
  let (leanRuntime, translation) ← OATP.Lean.Repl.translateToTPTP leanRuntime goal
  match translation with
  | .ok value =>
      if !value.problem.source.contains "fof(goal, conjecture" then
        throw <| IO.userError "Lean REPL translation omitted conjecture"
  | .error message => throw <| IO.userError s!"Lean REPL translation failed: {message}"
  let (_, reconstructed) ← OATP.Lean.Repl.reconstruct leanRuntime goal
      (.implicationIntro `h (.exact `h))
  match reconstructed with
  | .ok term =>
      if !term.checked || !term.term.contains "fun" then
        throw <| IO.userError "Lean REPL did not render a checked proof term"
  | .error message => throw <| IO.userError s!"Lean REPL reconstruction failed: {message}"
  IO.println "OATP tests passed"
  return 0
