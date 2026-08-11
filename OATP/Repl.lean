/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.TPTP
import OATP.Version
import OATP.Proof
import OATP.SystemOnTPTP
import OATP.Argus
import Argus

/-!
# OATP.Repl: pure interactive session state

The REPL keeps parsed TPTP data and derived symbol views. It does not perform IO or run provers;
the terminal runtime can therefore test command/state behavior without a TTY or network.
-/

namespace OATP.Repl

open OATP
open OATP.TPTP
open _root_.Argus

def helpTopics : List String :=
  ["cnf", "fof", "tff", "lean", "run", "context", "grammar", "roles"]

def grammarTopics : List String := OATP.TPTP.supportedTheories

def roleFormats : List String := OATP.TPTP.supportedTheories

def completionParam (typeName : String) (values : List String) : Param String :=
  Param.named typeName (Param.enum (values.map fun value => (value, value)))

def staticCompletionValues (typeName : String) : List String :=
  match typeName with
  | "TOPIC" => helpTopics
  | "FORMAT" => roleFormats
  | "THEORY" => OATP.TPTP.theoryChoices
  | "STEP" => OATP.Proof.stepNames
  | _ => []

inductive ProverReferenceKind where
  | local
  | online
  deriving BEq, DecidableEq, Repr, Inhabited

structure ProverReference where
  name : String
  kind : ProverReferenceKind
  deriving BEq, DecidableEq, Repr, Inhabited

namespace ProverReference

def fromLocal (name : String) : ProverReference := { name, kind := .local }

def fromOnline (name : String) : ProverReference :=
  { name := OATP.SystemOnTPTP.onlineSystemId name, kind := .online }

def ofString (name : String) : ProverReference :=
  if OATP.SystemOnTPTP.isOnlineReference name then fromOnline name else fromLocal name

def fromPersisted (value : String) : Option ProverReference :=
  if value.startsWith "local:" then some (fromLocal (value.drop "local:".length |>.toString))
  else if value.startsWith "online:" then some (fromOnline (value.drop "online:".length |>.toString))
  else if value.isEmpty then none else some (ofString value)

def persisted (reference : ProverReference) : String :=
  match reference.kind with
  | .local => "local:" ++ reference.name
  | .online => "online:" ++ reference.name

def display (reference : ProverReference) : String :=
  match reference.kind with
  | .local => reference.name
  | .online => OATP.SystemOnTPTP.onlineReference reference.name

end ProverReference

structure RunRequest where
  references : List ProverReference := []
  all : Bool := false
  endpoint : Option String := none
  refresh : Bool := false
  noCache : Bool := false
  timeout : Nat := OATP.defaultTimeoutSeconds
  maxOutput : Nat := OATP.defaultMaxOutputBytes
  arguments : List String := []
  deriving Repr

structure LocalRequest where
  executable : String
  timeout : Nat := OATP.defaultTimeoutSeconds
  maxOutput : Nat := OATP.defaultMaxOutputBytes
  arguments : List String := []
  deriving Repr

structure OnlineRequest where
  system : String
  endpoint : Option String := none
  timeout : Nat := OATP.defaultTimeoutSeconds
  maxOutput : Nat := OATP.defaultMaxOutputBytes
  deriving Repr

structure SystemsRequest where
  online : Bool := false
  endpoint : Option String := none
  refresh : Bool := false
  noCache : Bool := false
  deriving Repr

inductive Command where
  | help (topic : Option String)
  | history
  | state
  | stateTarget (target : String)
  | grammar (topic : String)
  | roles (topic : Option String)
  | version
  | clear
  | reset
  | parse (source : String)
  | axiom (name formula : String)
  | conjecture (name formula : String)
  | load (path : String)
  | goal (formula : String)
  | toLean (formula : String)
  | snapshot
  | toTptp
  | reconstruct (step : String)
  | term
  | run (request : RunRequest)
  | local (request : LocalRequest)
  | online (request : OnlineRequest)
  | theory (value : Option String)
  | prover (value : Option String)
  | provers
  | info (query : String)
  | theme (value : Option String)
  | systems (request : SystemsRequest)
  | doctor
  | quit
  | unknown (source : String)
  deriving Repr

inductive Submission where
  | command (value : Command)
  | source (value : String)
  deriving Repr

argus_opts RunOptions where
  references : List String := Spec.many (Spec.arg "REFERENCE" "Prover reference" Param.str);
  provers : List String := Spec.many (Spec.flag "prover" (some 'p')
    "Prover reference; repeat for a portfolio" (Param.named "PROVER" Param.str));
  catalogue : OATP.Argus.CatalogueOptions := OATP.Argus.CatalogueOptions.spec;
  all : Bool := Spec.switch "all" (some 'a') "Use all installed provers";
  resources : OATP.Argus.ResourceOptions := OATP.Argus.ResourceOptions.spec

argus_opts LocalOptions where
  executable : String := Spec.alt
    (Spec.flag "executable" (some 'x') "Local prover executable" Param.path)
    (Spec.arg "EXECUTABLE" "Local prover executable" Param.path);
  arguments : List String := Spec.many (Spec.arg "ARG" "Argument passed to the prover" Param.str);
  resources : OATP.Argus.ResourceOptions := OATP.Argus.ResourceOptions.spec

argus_opts OnlineOptions where
  system : String := Spec.alt
    (Spec.flag "system" (some 's') "SystemOnTPTP system label"
      (Param.named "SYSTEM" Param.str))
    (Spec.arg "SYSTEM" "SystemOnTPTP system label" (Param.named "SYSTEM" Param.str));
  remote : OATP.Argus.RemoteOptions := OATP.Argus.RemoteOptions.spec

argus_opts SystemsOptions where
  online : Bool := Spec.switch "online" (some 'o') "Also fetch online provers";
  catalogue : OATP.Argus.CatalogueOptions := OATP.Argus.CatalogueOptions.spec

private def parseSpec {α : Type} {g : Grade} (spec : Argus.Spec g α)
    (args : List String) : Except String α :=
  match Argus.run spec args with
  | .ok value => .ok value
  | .error errors => .error (String.intercalate "\n" (errors.map Argus.Err.message))

private def splitArguments : List String → List String × List String
  | [] => ([], [])
  | "__OATP_ARGS__" :: rest => ([], rest)
  | value :: rest =>
      let (before, after) := splitArguments rest
      (value :: before, after)

private def splitTerminator : List String → List String × Option (List String)
  | [] => ([], none)
  | "--" :: rest => ([], some rest)
  | value :: rest =>
      let (before, after) := splitTerminator rest
      (value :: before, after)

private def runRequestOf (options : RunOptions) : RunRequest :=
  let (references, arguments) := splitArguments options.references
  { references := (references ++ options.provers).map ProverReference.ofString
    all := options.all
    endpoint := options.catalogue.endpoint
    refresh := options.catalogue.refresh
    noCache := options.catalogue.noCache
    timeout := options.resources.timeout.getD OATP.defaultTimeoutSeconds
    maxOutput := options.resources.maxOutput.getD OATP.defaultMaxOutputBytes
    arguments }

private def localRequestOf (options : LocalOptions) : LocalRequest :=
  let (_, arguments) := splitArguments options.arguments
  { executable := options.executable
    timeout := options.resources.timeout.getD OATP.defaultTimeoutSeconds
    maxOutput := options.resources.maxOutput.getD OATP.defaultMaxOutputBytes
    arguments := if arguments.isEmpty then options.arguments else arguments }

private def onlineRequestOf (options : OnlineOptions) : OnlineRequest :=
  { system := options.system
    endpoint := options.remote.endpoint
    timeout := options.remote.resources.timeout.getD OATP.defaultTimeoutSeconds
    maxOutput := options.remote.resources.maxOutput.getD OATP.defaultMaxOutputBytes }

private def systemsRequestOf (options : SystemsOptions) : SystemsRequest :=
  { online := options.online, endpoint := options.catalogue.endpoint,
    refresh := options.catalogue.refresh, noCache := options.catalogue.noCache }

def parseRunRequest (args : List String) : Except String RunRequest :=
  let (args, tail) := splitTerminator args
  parseSpec RunOptions.spec args |>.map fun request =>
    { runRequestOf request with arguments := tail.getD (runRequestOf request).arguments }

def parseLocalRequest (args : List String) : Except String LocalRequest :=
  let (args, tail) := splitTerminator args
  parseSpec LocalOptions.spec args |>.map fun request =>
    { localRequestOf request with arguments := tail.getD (localRequestOf request).arguments }

def parseOnlineRequest (args : List String) : Except String OnlineRequest :=
  let (args, tail) := splitTerminator args
  match tail with
  | some (_ :: _) => .error "online commands do not accept arguments after --"
  | _ => parseSpec OnlineOptions.spec args |>.map onlineRequestOf

def parseSystemsRequest (args : List String) : Except String SystemsRequest :=
  let (args, tail) := splitTerminator args
  match tail with
  | some (_ :: _) => .error "systems does not accept arguments after --"
  | _ => parseSpec SystemsOptions.spec args |>.map systemsRequestOf

private def words (source : String) : List String :=
  source.splitOn " " |>.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def textSpec (name help : String) : Spec (1 * (1 * conditional * flexible)) String :=
  Spec.map (fun values : List String => String.intercalate " " values)
    (Spec.map2 (fun value values => value :: values)
      (Spec.arg name help (Param.named name Param.str))
      (Spec.many (Spec.arg name help (Param.named name Param.str))))

def commandSpec : Argus.Command Command :=
  Argus.group "oatp"
    [ Argus.cmd "help" (Spec.map Command.help (Spec.opt (Spec.arg "TOPIC" "Help topic"
        (Param.named "TOPIC" Param.str))))
        (description := "Show REPL help")
    , Argus.cmd "history" (Spec.const .history) (description := "Show command history")
    , Argus.cmd "state" (Spec.map (fun target => match target with
          | none => .state
          | some target => .stateTarget target)
        (Spec.opt (Spec.arg "TARGET" "Context target" (Param.named "TARGET" Param.str))))
        (description := "Open or focus the state drawer")
    , Argus.cmd "grammar" (Spec.map Command.grammar
        (Spec.arg "TOPIC" "Grammar topic" (completionParam "TOPIC" grammarTopics)))
        (description := "Show grammar help")
    , Argus.cmd "roles" (Spec.map Command.roles
        (Spec.opt (Spec.arg "FORMAT" "TPTP format" (completionParam "FORMAT" roleFormats))))
        (description := "Show role help")
    , Argus.cmd "version" (Spec.const .version) (description := "Show the OATP version")
    , Argus.cmd "clear" (Spec.const .clear) (description := "Clear the session context")
    , Argus.cmd "reset" (Spec.const .reset) (description := "Reset the complete session")
    , Argus.cmd "parse" (Spec.map Command.parse (textSpec "SOURCE" "TPTP source"))
        (description := "Parse TPTP source")
    , Argus.cmd "axiom" (Spec.map2 Command.axiom
        (Spec.arg "NAME" "Statement name" Param.str) (textSpec "FORMULA" "Formula"))
        (description := "Add an axiom")
    , Argus.cmd "conjecture" (Spec.map2 Command.conjecture
        (Spec.arg "NAME" "Statement name" Param.str) (textSpec "FORMULA" "Formula"))
        (description := "Add a conjecture")
    , Argus.cmd "load" (Spec.map Command.load (Spec.arg "PATH" "TPTP file" Param.path))
        (description := "Load a TPTP file")
    , Argus.cmd "goal" (Spec.map Command.goal (textSpec "FORMULA" "Lean formula"))
        (description := "Create a Lean goal")
    , Argus.cmd "to-lean" (Spec.map Command.toLean (textSpec "FORMULA" "Lean formula"))
        (description := "Create a Lean goal")
    , Argus.cmd "translate-to-lean"
        (Spec.map Command.toLean (textSpec "FORMULA" "Lean formula"))
        (description := "Create a Lean goal")
    , Argus.cmd "snapshot" (Spec.const .snapshot) (description := "Show the current Lean goal")
    , Argus.cmd "to-tptp" (Spec.const .toTptp) (description := "Translate the Lean goal to TPTP")
    , Argus.cmd "reconstruct" (Spec.map Command.reconstruct (textSpec "STEP" "Proof step"))
        (description := "Reconstruct and check a proof step")
    , Argus.cmd "term" (Spec.const .term) (description := "Show the checked term")
    , Argus.cmd "run" (Spec.map (fun options => .run (runRequestOf options)) RunOptions.spec)
        (description := "Run selected provers")
    , Argus.cmd "local" (Spec.map (fun options => .local (localRequestOf options)) LocalOptions.spec)
        (description := "Run a local prover")
    , Argus.cmd "online"
        (Spec.map (fun options => .online (onlineRequestOf options)) OnlineOptions.spec)
        (description := "Run an online prover")
    , Argus.cmd "theory" (Spec.map Command.theory
        (Spec.opt (Spec.arg "THEORY" "Theory" (completionParam "THEORY"
          OATP.TPTP.theoryChoices))))
        (description := "Show or select the TPTP theory")
    , Argus.cmd "prover" (Spec.map Command.prover
        (Spec.opt (Spec.arg "PROVER" "Default prover" (Param.named "PROVER" Param.str))))
        (description := "Show or select the default prover")
    , Argus.cmd "provers" (Spec.const .provers) (description := "Select enabled provers")
    , Argus.cmd "info" (Spec.map Command.info
        (Spec.arg "PROVER" "Prover name" (Param.named "PROVER" Param.str)))
        (description := "Show prover information")
    , Argus.cmd "theme" (Spec.map Command.theme
        (Spec.opt (Spec.arg "THEME" "Color theme" (Param.named "THEME" Param.str))))
        (description := "Show or select the color theme")
    , Argus.cmd "systems"
        (Spec.map (fun options => .systems (systemsRequestOf options)) SystemsOptions.spec)
        (description := "List available provers")
    , Argus.cmd "doctor" (Spec.const .doctor) (description := "Check runtime readiness")
    , Argus.cmd "quit" (Spec.const .quit) (description := "Leave the REPL")
    , Argus.cmd "exit" (Spec.const .quit) (description := "Leave the REPL") ]

private def commandArgv (source : String) : List String × Option (List String) :=
  match words source.trimAscii.toString with
  | command :: args =>
      let command := (command.drop 1).toString
      let (args, tail) := splitTerminator args
      (command :: args, tail)
  | [] => ([], none)

private def appendCommandArguments (command : Command) (tail : Option (List String)) :
    Except String Command :=
  match tail with
  | none => .ok command
  | some arguments =>
      match command with
      | .run request => .ok (.run { request with arguments })
      | .local request => .ok (.local { request with arguments })
      | .online _ =>
          if arguments.isEmpty then .ok command
          else .error "online commands do not accept arguments after --"
      | _ =>
          if arguments.isEmpty then .ok command
          else .error "command does not accept arguments after --"

def parseCommandSpec (source : String) : Except String Command :=
  let line := source.trimAscii.toString
  if !line.startsWith "/" then .error "commands start with `/`; try `/help`"
  else
    let (argv, tail) := commandArgv line
    match commandSpec.run argv with
    | .ok command => appendCommandArguments command tail
    | .error errors =>
        let message := String.intercalate "\n" (errors.map Argus.Err.message)
        let hint := match argv with
          | command :: _ => s!"\ntry `/help {command}` for usage and examples"
          | [] => ""
        .error (message ++ hint)

private def commandChildren : List (Argus.Command Command) :=
  match commandSpec.body with
  | .subs children => children
  | .opts _ => []

def commandNames : List String := commandChildren.map (·.name)

private def usageArguments : {g : Grade} → {α : Type} → Spec g α → List (String × Bool × Bool)
  | _, _, .const _ | _, _, .switch _ _ _ | _, _, .flag _ _ _ _ => []
  | _, _, .arg name _ _ => [(name, false, false)]
  | _, _, .ap function argument => usageArguments function ++ usageArguments argument
  | _, _, .alt left right => usageArguments left ++ usageArguments right
  | _, _, .opt argument => usageArguments argument |>.map fun (name, _, variadic) =>
      (name, true, variadic)
  | _, _, .many argument => usageArguments argument |>.map fun (name, _, _) =>
      (name, true, true)

private def usageLine (command : Argus.Command Command) : String :=
  let flags := if command.toMeta.flags.isEmpty then "" else " [OPTIONS]"
  let arguments := match command.body with
    | .opts spec => usageArguments spec
    | .subs _ => []
  let arguments := arguments.foldl (fun output (name, optional, variadic) =>
    let value := "<" ++ name ++ ">" ++ if variadic then "..." else ""
    output ++ if optional then " [" ++ value ++ "]" else " " ++ value) ""
  command.name ++ flags ++ arguments

def commandHelpText : String :=
  String.intercalate "\n" <| ["OATP REPL commands"] ++ commandChildren.map fun command =>
    "  /" ++ usageLine command ++ "  " ++ command.description

inductive SymbolKind where
  | variable
  | constant
  | function
  | predicate
  deriving BEq, DecidableEq, Repr

structure Symbol where
  kind : SymbolKind
  name : String
  arity : Nat := 0
  deriving BEq, DecidableEq, Repr

structure FormulaView where
  cell : Nat
  name : String
  kind : String
  role : String
  source : String
  formula : String
  symbols : Array Symbol := #[]
  deriving Repr

structure HistoryEntry where
  cell : Nat
  input : String
  result : String
  deriving Repr

structure Session where
  nextCell : Nat := 1
  problemSource : String := ""
  formulas : Array FormulaView := #[]
  symbols : Array Symbol := #[]
  history : Array HistoryEntry := #[]
  deriving Repr

def parseCommand (source : String) : Command :=
  let line := source.trimAscii.toString
  match parseCommandSpec line with
  | .ok command => command
  | .error _ => .unknown line

def parseInput (source : String) : Submission :=
  if source.trimAscii.toString.startsWith "/" then
    .command (parseCommand source)
  else
    .source source

private def addSymbol (symbols : Array Symbol) (symbol : Symbol) : Array Symbol :=
  if symbols.any (· == symbol) then symbols else symbols.push symbol

private partial def collectTerm (term : _root_.TPTP.Formula.Term) (symbols : Array Symbol) :
    Array Symbol :=
  match term with
  | .var name => addSymbol symbols { kind := .variable, name }
  | .constant name => addSymbol symbols { kind := .constant, name }
  | .function name arguments =>
      let symbols := addSymbol symbols { kind := .function, name, arity := arguments.size }
      arguments.foldl (fun symbols term => collectTerm term symbols) symbols

private partial def collectFormula (formula : _root_.TPTP.Formula.Expr)
    (symbols : Array Symbol) : Array Symbol :=
  match formula with
  | .atom predicate arguments =>
      let symbols := addSymbol symbols { kind := .predicate, name := predicate, arity := arguments.size }
      arguments.foldl (fun symbols term => collectTerm term symbols) symbols
  | .truth | .falsity => symbols
  | .not body => collectFormula body symbols
  | .and left right | .or left right | .implies left right | .iff left right =>
      collectFormula right (collectFormula left symbols)
  | .forall variables body | .exists variables body =>
      let symbols := variables.foldl (fun symbols name =>
        addSymbol symbols { kind := .variable, name }) symbols
      collectFormula body symbols

private def formulaView (cell : Nat) (statement : _root_.TPTP.Statement) : FormulaView :=
  match OATP.TPTP.Statement.parseFormula statement with
  | .ok formula =>
      { cell
        name := s!"{statement.name}"
        kind := s!"{statement.kind}"
        role := s!"{statement.role}"
        source := statement.render
        formula := match formula.toTPTP with
          | .ok rendered => rendered
          | .error _ => statement.formula
        symbols := collectFormula formula #[] }
  | .error _ =>
      { cell
        name := s!"{statement.name}"
        kind := s!"{statement.kind}"
        role := s!"{statement.role}"
        source := statement.render
        formula := statement.formula }

private def viewsOf (cell : Nat) (document : _root_.TPTP.Document) : Array FormulaView :=
  let views := document.items.toList.filterMap fun item =>
    match item with
    | .statement statement => some (formulaView cell statement)
    | .include _ => none
  views.toArray

private def appendSource (old source : String) : String :=
  if old.isEmpty then source else old ++ "\n" ++ source

private def record (session : Session) (input result : String) : Session :=
  { session with
    nextCell := session.nextCell + 1
    history := session.history.push { cell := session.nextCell, input, result } }

def note (session : Session) (input result : String) : Session :=
  record session input result

def addDocument (session : Session) (input : String) (document : _root_.TPTP.Document) : Session :=
  let views := viewsOf session.nextCell document
  let symbols := views.foldl (fun symbols view =>
    view.symbols.foldl addSymbol symbols) session.symbols
  let rendered := document.render
  let session := { session with
    problemSource := appendSource session.problemSource rendered
    formulas := session.formulas ++ views
    symbols }
  record session input s!"parsed {views.size} statement(s)"

def helpText : String :=
  commandHelpText ++ "\n" ++ String.intercalate "\n" [
    "Commands start with `/`; <ARG> is required and [ARG] is optional.",
    "  TAB completes command names, options, and paths",
    "examples: /run   /local eprover   /online online-vampire   /to-lean p => p",
    "topics: /help cnf   /help fof   /help tff   /help lean",
    "        /help run   /help context   /help grammar   /help roles",
    "grammar: ~p  p & q  p | q  p => q  p <=> q  ![X] : p(X)"
  ]

private def cnfHelp : String :=
  String.intercalate "\n" [
    "CNF — clause normal form",
    "statement: cnf(NAME, ROLE, CLAUSE).",
    "clause:    literal | (literal | literal | ...)",
    "literal:   atom | ~atom",
    "atom:      predicate | predicate(term, ...)",
    "terms:     constant | variable | function(term, ...)",
    "variables are implicitly universal; CNF uses only | and ~",
    "roles: axiom, hypothesis, definition, assumption, lemma, theorem,",
    "       corollary, conjecture, negated_conjecture, plain",
    "",
    "example: cnf(c1, axiom, p(a) | ~q(a)).",
    "example: cnf(goal, conjecture, mortal(socrates)).",
    "workflow:",
    "  /parse cnf(ax, axiom, p(a)).",
    "  /parse cnf(goal, conjecture, p(a)).",
    "  /state formulas   /roles cnf   /run --prover eprover"
  ]

private def fofHelp : String :=
  String.intercalate "\n" [
    "FOF — first-order formulas",
    "statement: fof(NAME, ROLE, FORMULA).",
    "formula: atom | ~F | (F & F) | (F | F) | (F => F) | (F <=> F)",
    "quantifiers: ![X] : F   ?[X] : F",
    "terms: constants, variables, and functions such as f(a, X)",
    "",
    "example: fof(ax, axiom, ![X] : (human(X) => mortal(X))).",
    "example: fof(goal, conjecture, mortal(socrates)).",
    "workflow:",
    "  /parse fof(ax, axiom, ![X] : (human(X) => mortal(X))).",
    "  /parse fof(fact, axiom, human(socrates)).",
    "  /parse fof(goal, conjecture, mortal(socrates)).",
    "  /run --prover eprover",
    "use FOF when the problem needs implication, conjunction, or quantifiers"
  ]

private def tffHelp : String :=
  String.intercalate "\n" [
    "TFF — typed first-order formulas",
    "type:     tff(nat_type, type, nat: $tType).",
    "constant: tff(zero_type, type, zero: nat).",
    "function: tff(add_type, type, add: (nat * nat) > nat).",
    "formula:  tff(goal, conjecture, ![X:nat] : add(X,zero) = X).",
    "connectives: ~  &  |  =>  <=>; quantifiers: ![...] and ?[...]",
    "",
    "try: /parse tff(nat_type, type, nat: $tType).",
    "TFF is parsed as TPTP input; use /state to inspect collected symbols"
  ]

private def grammarHelp : String :=
  String.intercalate "\n" [
    "Grammar lookup",
    "/grammar cnf       clause normal form",
    "/grammar fof       first-order formulas",
    "/grammar tff       typed first-order formulas",
    "/roles [cnf|fof|tff]  statement roles",
    "",
    "operators: ~  &  |  =>  <=>",
    "quantifiers: ![X] : F   ?[X] : F",
    "term shape: constant | variable | function(term, ...)",
    "Use /help cnf, /help fof, or /help tff for examples."
  ]

private def roleHelp (topic : Option String) : String :=
  let format := topic.getD "all"
  String.intercalate "\n" [
    s!"Roles ({format})",
    "axiom          accepted premise",
    "hypothesis     temporary premise",
    "definition     definitional statement",
    "assumption     assumed premise",
    "lemma          supporting result",
    "theorem        proved result",
    "corollary      consequence of a theorem",
    "conjecture     statement to prove",
    "negated_conjecture  refutation form",
    "plain          ordinary formula",
    "type           TFF type declaration",
    "interpretation / logic / unknown / fi_domain / fi_functors / fi_predicates",
    "",
    "roles are shared by CNF and FOF; TFF additionally uses `type`.",
    s!"Use /grammar {format} for the syntax."
  ]

private def leanHelp : String :=
  String.intercalate "\n" [
    "Lean bridge",
    "1. /goal p => p              create a Lean goal",
    "2. /snapshot                  show variables and target",
    "3. /to-tptp                   translate the goal to TPTP",
    "4. /reconstruct implication-intro h exact h",
    "5. /term                      show the kernel-checked term",
    "",
    "atoms become Prop variables; &&, ||, ~, => and <=> are supported",
    "example result: fun h => h : _fvar.1 → _fvar.1"
  ]

private def runHelp : String :=
  String.intercalate "\n" [
    "Provers",
    "/run                    run the configured or first local prover",
    "/run [--prover NAME] [--all] [--timeout SEC] [--max-output BYTES]",
    "     [--refresh] [--no-cache] [--endpoint URL]",
    "/run --all              run every installed local prover",
    "/local EXECUTABLE [--timeout SEC] [--max-output BYTES] [-- ARGUMENTS...]",
    "/online SYSTEM [--endpoint URL] [--timeout SEC] [--max-output BYTES]",
    "/provers                local + online selection drawer; online starts unchecked",
    "/systems [--online] [--refresh] [--no-cache] [--endpoint URL]",
    "/doctor                  check transports and local provers",
    "",
    "interactive run drawer:",
    "  Ctrl-R                   open the latest run; H returns to input",
    "  J/K or ↑/↓               focus a prover result",
    "",
    "workflow (a problem is required):",
    "  /parse fof(goal, conjecture, p => p).",
    "  /run --prover eprover",
    "  /local vampire -- --mode casc"
  ]

private def contextHelp : String :=
  String.intercalate "\n" [
    "Context drawer",
    "/state [TARGET]            open context or focus a box",
    "/state goal|formulas|symbols|problem|translation|term|all",
    "J/K or ↑/↓                  move between boxes",
    "Enter/Space                open or close the focused box",
    "→ / ←                      expand or collapse",
    "Ctrl-]                     focus the visible drawer",
    "H                          return focus to the main input",
    "Ctrl-R                     open the latest prover run",
    "mouse click                focus/toggle a box; scroll changes focus",
    "boxes: formulas, symbols, problem, Lean goal, Lean → TPTP, checked term",
    "theory: /theory fof|cnf|tff (tf1 alias); provers: /provers; theme: /theme NAME"
  ]

private def commandHelp (command : Argus.Command Command) : String :=
  let details := match command.name with
    | "goal" | "to-lean" | "translate-to-lean" | "snapshot" | "to-tptp" | "reconstruct" | "term" =>
        leanHelp
    | "run" | "local" | "online" | "prover" | "provers" | "systems" | "doctor" => runHelp
    | "state" | "history" => contextHelp
    | "parse" | "axiom" | "conjecture" | "grammar" | "roles" => grammarHelp
    | _ => ""
  String.intercalate "\n" <| [s!"/{usageLine command}", command.description] ++
    (if details.isEmpty then [] else ["", details])

private def normalizeHelpTopic (topic : String) : String :=
  let topic := topic.trimAscii.toString
  if topic.startsWith "/" then topic.drop 1 |>.trimAscii.toString else topic

def helpFor : Option String → String
  | none => helpText
  | some rawTopic =>
      let topic := normalizeHelpTopic rawTopic
      match topic.toLower with
      | "cnf" => cnfHelp
      | "fof" => fofHelp
      | "tff" => tffHelp
      | "lean" => leanHelp
      | "run" | "provers" => runHelp
      | "context" | "state" => contextHelp
      | "grammar" => grammarHelp
      | "roles" => roleHelp none
      | topic =>
          match commandChildren.find? (fun command => command.name == topic) with
          | some command => commandHelp command
          | none =>
              if topic.startsWith "roles " then
                roleHelp (some (topic.drop "roles ".length |>.trimAscii.toString))
              else String.intercalate "\n" [
                s!"unknown help topic `{topic}`",
                "try: /help cnf, /help fof, /help tff, /help lean, /help to-lean, /help run, /help context"
              ]

def parseSource (session : Session) (input source : String) : Except String Session :=
  match OATP.TPTP.parse source with
  | .ok document => pure (addDocument session input document)
  | .error error => .error (error.pretty source.toUTF8)

private def addFormulaCommand (session : Session) (input name role formula : String) :
    Except String Session :=
  parseSource session input s!"fof({name}, {role}, {formula})."

def apply (session : Session) (input : String) : Except String Session :=
  match parseInput input with
  | .source source => parseSource session source source
  | .command command =>
      match command with
      | .help topic => pure (record session input (helpFor topic))
      | .history => pure (record session input s!"{session.history.size} history entries")
      | .state => pure (record session input s!"{session.formulas.size} formulas, {session.symbols.size} symbols")
      | .stateTarget target => pure (record session input s!"state target: {target}")
      | .grammar topic => pure (record session input (helpFor (some topic)))
      | .roles topic => pure (record session input (roleHelp topic))
      | .version => pure (record session input s!"oatp {OATP.version}")
      | .clear => pure (record { session with formulas := #[], symbols := #[], problemSource := "" }
          input "session context cleared")
      | .reset => pure (record {} input "session reset")
      | .parse source => parseSource session input source
      | .axiom name formula => addFormulaCommand session input name "axiom" formula
      | .conjecture name formula => addFormulaCommand session input name "conjecture" formula
      | .load path => pure (record session input s!"load requested: {path}")
      | .goal _ | .toLean _ => pure (record session input "Lean goal requested")
      | .snapshot => pure (record session input "Lean snapshot requested")
      | .toTptp => pure (record session input "TPTP translation requested")
      | .reconstruct _ => pure (record session input "proof reconstruction requested")
      | .term => pure (record session input "checked term requested")
      | .run _ | .local _ | .online _ => pure (record session input "prover run requested")
      | .theory none => pure (record session input "theory requested")
      | .theory (some value) => pure (record session input s!"theory requested: {value}")
      | .prover none => pure (record session input "prover requested")
      | .prover (some value) => pure (record session input s!"prover requested: {value}")
      | .provers => pure (record session input "prover selection requested")
      | .info query => pure (record session input s!"prover info requested: {query}")
      | .theme none => pure (record session input "theme requested")
      | .theme (some value) => pure (record session input s!"theme requested: {value}")
      | .systems _ => pure (record session input "systems requested")
      | .doctor => pure (record session input "doctor requested")
      | .quit => pure (record session input "quit requested")
      | .unknown source => .error s!"unknown REPL command `{source}`"

def problem (session : Session) : Option Problem :=
  if session.problemSource.isEmpty then none else some {
    name := "oatp-repl"
    source := session.problemSource
  }

end OATP.Repl
