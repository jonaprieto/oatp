/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.TPTP

/-!
# OATP.Repl: pure interactive session state

The REPL keeps parsed TPTP data and derived symbol views. It does not perform IO or run provers;
the terminal runtime can therefore test command/state behavior without a TTY or network.
-/

namespace OATP.Repl

open OATP
open OATP.TPTP

inductive Command where
  | help
  | history
  | state
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
  if line == "/help" then .help
  else if line == "/history" then .history
  else if line == "/state" then .state
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
      | .help => pure (record session input "commands: /axiom /conjecture /parse /state /history")
      | .history => pure (record session input s!"{session.history.size} history entries")
      | .state => pure (record session input s!"{session.formulas.size} formulas, {session.symbols.size} symbols")
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
