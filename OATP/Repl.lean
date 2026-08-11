/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.TPTP
import OATP.Version

/-!
# OATP.Repl: pure interactive session state

The REPL keeps parsed TPTP data and derived symbol views. It does not perform IO or run provers;
the terminal runtime can therefore test command/state behavior without a TTY or network.
-/

namespace OATP.Repl

open OATP
open OATP.TPTP

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
  | unknown (source : String)
  deriving Repr

inductive Submission where
  | command (value : Command)
  | source (value : String)
  deriving Repr

structure RunRequest where
  references : List String := []
  all : Bool := false
  endpoint : Option String := none
  refresh : Bool := false
  noCache : Bool := false
  timeout : Nat := 30
  maxOutput : Nat := 4 * 1024 * 1024
  arguments : List String := []
  deriving Repr

structure LocalRequest where
  executable : String
  timeout : Nat := 30
  maxOutput : Nat := 4 * 1024 * 1024
  arguments : List String := []
  deriving Repr

structure OnlineRequest where
  system : String
  endpoint : Option String := none
  timeout : Nat := 30
  maxOutput : Nat := 4 * 1024 * 1024
  deriving Repr

structure SystemsRequest where
  online : Bool := false
  endpoint : Option String := none
  refresh : Bool := false
  noCache : Bool := false
  deriving Repr

private def parseNatOption (flag value : String) : Except String Nat :=
  match value.toNat? with
  | some number => pure number
  | none => .error s!"{flag} expects a non-negative integer, got `{value}`"

def parseRunRequest (args : List String) : Except String RunRequest :=
  let rec loop (args : List String) (request : RunRequest) : Except String RunRequest :=
    match args with
    | [] => pure request
    | "--prover" :: reference :: rest =>
        loop rest { request with references := request.references ++ [reference] }
    | "--prover" :: [] => .error "--prover expects a reference"
    | "--endpoint" :: endpoint :: rest => loop rest { request with endpoint := some endpoint }
    | "--endpoint" :: [] => .error "--endpoint expects a URL"
    | "--refresh" :: rest => loop rest { request with refresh := true }
    | "--no-cache" :: rest => loop rest { request with noCache := true }
    | "--all" :: rest => loop rest { request with all := true }
    | "--timeout" :: value :: rest => do
        let timeout ← parseNatOption "--timeout" value
        loop rest { request with timeout }
    | "--timeout" :: [] => .error "--timeout expects a value"
    | "--max-output" :: value :: rest => do
        let maxOutput ← parseNatOption "--max-output" value
        loop rest { request with maxOutput }
    | "--max-output" :: [] => .error "--max-output expects a value"
    | "--" :: rest => pure { request with arguments := rest }
    | reference :: rest => loop rest { request with references := request.references ++ [reference] }
  loop args {}

def parseLocalRequest (args : List String) : Except String LocalRequest :=
  let rec loop (args : List String) (request : Option LocalRequest)
      (timeout maxOutput : Nat) : Except String LocalRequest :=
    match args with
    | [] =>
        match request with
        | some request => pure { request with timeout, maxOutput }
        | none => .error "/local expects an executable"
    | "--executable" :: executable :: rest =>
        loop rest (some { executable }) timeout maxOutput
    | "--executable" :: [] => .error "--executable expects a path"
    | "--timeout" :: value :: rest => do
        let timeout ← parseNatOption "--timeout" value
        loop rest request timeout maxOutput
    | "--timeout" :: [] => .error "--timeout expects a value"
    | "--max-output" :: value :: rest => do
        let maxOutput ← parseNatOption "--max-output" value
        loop rest request timeout maxOutput
    | "--max-output" :: [] => .error "--max-output expects a value"
    | "--" :: rest =>
        match request with
        | some request => pure { request with timeout, maxOutput, arguments := rest }
        | none => .error "/local expects an executable before --"
    | value :: rest =>
        match request with
        | some request => pure { request with timeout, maxOutput, arguments := value :: rest }
        | none => loop rest (some { executable := value }) timeout maxOutput
  loop args none 30 (4 * 1024 * 1024)

def parseOnlineRequest (args : List String) : Except String OnlineRequest :=
  let rec loop (args : List String) (system endpoint : Option String)
      (timeout maxOutput : Nat) : Except String OnlineRequest :=
    match args with
    | [] =>
        match system with
        | some system => pure { system, endpoint, timeout, maxOutput }
        | none => .error "/online expects a system reference"
    | "--system" :: value :: rest => loop rest (some value) endpoint timeout maxOutput
    | "--system" :: [] => .error "--system expects a reference"
    | "--endpoint" :: value :: rest => loop rest system (some value) timeout maxOutput
    | "--endpoint" :: [] => .error "--endpoint expects a URL"
    | "--timeout" :: value :: rest => do
        let timeout ← parseNatOption "--timeout" value
        loop rest system endpoint timeout maxOutput
    | "--timeout" :: [] => .error "--timeout expects a value"
    | "--max-output" :: value :: rest => do
        let maxOutput ← parseNatOption "--max-output" value
        loop rest system endpoint timeout maxOutput
    | "--max-output" :: [] => .error "--max-output expects a value"
    | value :: rest =>
        match system with
        | some _ => .error s!"unexpected online option `{value}`"
        | none => loop rest (some value) endpoint timeout maxOutput
  loop args none none 30 (4 * 1024 * 1024)

def parseSystemsRequest (args : List String) : Except String SystemsRequest :=
  let rec loop (args : List String) (request : SystemsRequest) : Except String SystemsRequest :=
    match args with
    | [] => pure request
    | "--online" :: rest => loop rest { request with online := true }
    | "--refresh" :: rest => loop rest { request with refresh := true }
    | "--no-cache" :: rest => loop rest { request with noCache := true }
    | "--endpoint" :: endpoint :: rest => loop rest { request with endpoint := some endpoint }
    | "--endpoint" :: [] => .error "--endpoint expects a URL"
    | value :: _ => .error s!"unexpected systems option `{value}`"
  loop args {}

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

private def words (source : String) : List String :=
  source.splitOn " " |>.map (·.trimAscii.toString) |>.filter (!·.isEmpty)

private def restAfter (marker source : String) : String :=
  (source.drop marker.length).trimAscii.toString

private def nameAndFormula (source : String) : Option (String × String) :=
  match words source with
  | name :: formula => some (name, String.intercalate " " formula)
  | _ => none

def parseCommand (source : String) : Command :=
  let line := source.trimAscii.toString
  if line == "/help" then .help none
  else if line.startsWith "/help " then .help (some (restAfter "/help " line))
  else if line == "/history" then .history
  else if line == "/state" then .state
  else if line.startsWith "/state " then .stateTarget (restAfter "/state " line)
  else if line.startsWith "/grammar " then .grammar (restAfter "/grammar " line)
  else if line == "/roles" then .roles none
  else if line.startsWith "/roles " then .roles (some (restAfter "/roles " line))
  else if line == "/version" then .version
  else if line == "/clear" then .clear
  else if line == "/reset" then .reset
  else if line.startsWith "/parse " then .parse (restAfter "/parse " line)
  else if line.startsWith "/axiom " then
    match nameAndFormula (restAfter "/axiom " line) with
    | some (name, formula) => .axiom name formula
    | none => .unknown line
  else if line.startsWith "/conjecture " then
    match nameAndFormula (restAfter "/conjecture " line) with
    | some (name, formula) => .conjecture name formula
    | none => .unknown line
  else .unknown line

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
  String.intercalate "\n" [
    "OATP REPL help",
    "  /load FILE       load a TPTP problem",
    "  /parse SOURCE    parse TPTP text",
    "  /axiom N F       add an axiom; /conjecture N F",
    "  /goal F          create a Lean goal; /to-lean F",
    "  /snapshot /to-tptp /reconstruct S /term",
    "  /run OPTIONS     test with provers; /local; /online",
    "  /state [TARGET] /history /clear /reset /systems /doctor /version /quit",
    "  /grammar TOPIC /roles [FORMAT] /theory /prover /provers /info /theme",
    "  TAB completes finite command arguments and prover/options names",
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
    "try: /parse cnf(c1, axiom, p(a) | ~q(a)).",
    "then: /state formulas   /roles cnf   /run --prover eprover"
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
    "try: /parse fof(ax, axiom, ![X] : (human(X) => mortal(X))).",
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
    "/run [--prover NAME] [--all] [--timeout SEC] [--max-output BYTES]",
    "     [--refresh] [--no-cache] [--endpoint URL]",
    "/run --all              run every installed local prover",
    "/local EXECUTABLE [--timeout SEC] [--max-output BYTES] [-- ARGUMENTS...]",
    "/online SYSTEM [--endpoint URL] [--timeout SEC] [--max-output BYTES]",
    "/systems [--online] [--refresh] [--no-cache] [--endpoint URL]",
    "/doctor                  check transports and local provers",
    "",
    "examples: /run --prover eprover",
    "          /local vampire -- --mode casc"
  ]

private def contextHelp : String :=
  String.intercalate "\n" [
    "Context drawer",
    "/state [TARGET]            open context or focus a box",
    "/state goal|formulas|symbols|problem|translation|term|all",
    "J/K or ↑/↓                  move between boxes",
    "Enter/Space                open or close the focused box",
    "→ / ←                      expand or collapse",
    "H                          return to the main panel",
    "mouse click                focus/toggle a box; scroll changes focus",
    "boxes: formulas, symbols, problem, Lean goal, Lean → TPTP, checked term",
    "theory: /theory fof|cnf|tff (tf1 alias); provers: /provers; theme: /theme NAME"
  ]

def helpFor : Option String → String
  | none => helpText
  | some topic => match topic.toLower with
      | "cnf" => cnfHelp
      | "fof" => fofHelp
      | "tff" => tffHelp
      | "lean" => leanHelp
      | "run" | "provers" => runHelp
      | "context" | "state" => contextHelp
      | "grammar" => grammarHelp
      | "roles" => roleHelp none
      | topic =>
          if topic.startsWith "roles " then
            roleHelp (some (topic.drop "roles ".length |>.trimAscii.toString))
          else String.intercalate "\n" [
            s!"unknown help topic `{topic}`",
            "try: /help cnf, /help fof, /help tff, /help lean, /help run, /help context"
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
      | .unknown source => .error s!"unknown REPL command `{source}`"

def problem (session : Session) : Option Problem :=
  if session.problemSource.isEmpty then none else some {
    name := "oatp-repl"
    source := session.problemSource
  }

end OATP.Repl
