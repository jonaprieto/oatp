/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0.
-/

import OATP.TPTP

/-!
# OATP.Repl.Context

Pure indexed TPTP context state for the REPL. Commands and terminal rendering live in sibling
modules; this module owns the authoritative context and its derived views.
-/

namespace OATP.Repl

open OATP
open OATP.TPTP

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
  id : Nat := 0
  deriving Repr

structure ContextItem where
  id : Nat
  cell : Nat
  value : _root_.TPTP.Item
  deriving Repr

structure HistoryEntry where
  cell : Nat
  input : String
  result : String
  deriving Repr

structure Session where
  nextCell : Nat := 1
  nextContextId : Nat := 1
  problemSource : String := ""
  formulas : Array FormulaView := #[]
  symbols : Array Symbol := #[]
  context : Array ContextItem := #[]
  history : Array HistoryEntry := #[]
  deriving Repr

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
      let symbols := addSymbol symbols
        { kind := .predicate, name := predicate, arity := arguments.size }
      arguments.foldl (fun symbols term => collectTerm term symbols) symbols
  | .truth | .falsity => symbols
  | .not body => collectFormula body symbols
  | .and left right | .or left right | .implies left right | .iff left right =>
      collectFormula right (collectFormula left symbols)
  | .forall variables body | .exists variables body =>
      let symbols := variables.foldl (fun symbols name =>
        addSymbol symbols { kind := .variable, name }) symbols
      collectFormula body symbols

private def formulaView (id cell : Nat) (statement : _root_.TPTP.Statement) : FormulaView :=
  match OATP.TPTP.Statement.parseFormula statement with
  | .ok formula =>
      { id, cell
        name := s!"{statement.name}"
        kind := s!"{statement.kind}"
        role := s!"{statement.role}"
        source := statement.render
        formula := match formula.toTPTP with
          | .ok rendered => rendered
          | .error _ => statement.formula
        symbols := collectFormula formula #[] }
  | .error _ =>
      { id, cell
        name := s!"{statement.name}"
        kind := s!"{statement.kind}"
        role := s!"{statement.role}"
        source := statement.render
        formula := statement.formula }

private def contextItems (nextId cell : Nat) (document : _root_.TPTP.Document) :
    Array ContextItem × Nat :=
  document.items.foldl (fun (items, nextId) item =>
    (items.push { id := nextId, cell, value := item }, nextId + 1)) (#[], nextId)

private def viewsOf (items : Array ContextItem) : Array FormulaView :=
  items.foldl (fun views item =>
    match item.value with
    | .statement statement => views.push (formulaView item.id item.cell statement)
    | .include _ => views) #[]

private def symbolsOf (views : Array FormulaView) : Array Symbol :=
  views.foldl (fun symbols view => view.symbols.foldl addSymbol symbols) #[]

private def problemSourceOf (items : Array ContextItem) : String :=
  String.intercalate "\n" (items.toList.map (fun item => item.value.render))

private def rebuildContext (session : Session) : Session :=
  let formulas := viewsOf session.context
  { session with
    problemSource := problemSourceOf session.context
    formulas
    symbols := symbolsOf formulas }

private def record (session : Session) (input result : String) : Session :=
  { session with
    nextCell := session.nextCell + 1
    history := session.history.push { cell := session.nextCell, input, result } }

def note (session : Session) (input result : String) : Session :=
  record session input result

def addDocument (session : Session) (input : String) (document : _root_.TPTP.Document) : Session :=
  let (items, nextContextId) := contextItems session.nextContextId session.nextCell document
  let session := rebuildContext { session with
    context := session.context ++ items
    nextContextId }
  record session input s!"parsed {viewsOf items |>.size} statement(s)"

def clearContext (session : Session) : Session :=
  rebuildContext { session with context := #[] }

private def validateStatement (statement : _root_.TPTP.Statement) : Except String Unit :=
  match statement.kind, statement.role with
  | .cnf, .conjecture =>
      .error ("CNF does not support the `conjecture` role; use " ++
        "`negated_conjecture` with a negated clause")
  | _, _ => pure ()

private def validateDocument (document : _root_.TPTP.Document) : Except String Unit :=
  document.items.toList.mapM (fun item => match item with
    | _root_.TPTP.Item.statement statement => validateStatement statement
    | _root_.TPTP.Item.include _ => pure ()) |>.map (fun _ => ())

private def parseDocument (source : String) : Except String _root_.TPTP.Document := do
  match OATP.TPTP.parse source with
  | .error error => .error (error.pretty source.toUTF8)
  | .ok document =>
      validateDocument document
      pure document

def parseSource (session : Session) (input source : String) : Except String Session :=
  match parseDocument source with
  | .ok document => pure (addDocument session input document)
  | .error message => .error message

private def contextIndexText (index : Nat) : String := s!"#{index}"

private def missingContextIndices (session : Session) (indices : List Nat) : List Nat :=
  indices.filter (fun index => !session.context.any (·.id == index))

def removeContext (session : Session) (input : String) (indices : List Nat) :
    Except String Session :=
  let indices := indices.eraseDups
  if indices.isEmpty then
    .error "remove expects at least one context index"
  else
    match missingContextIndices session indices with
    | missing :: rest =>
        let missing := String.intercalate ", " (missing :: rest |>.map contextIndexText)
        .error s!"unknown context index {missing}"
    | [] =>
        let context := session.context.filter (fun item => !indices.contains item.id)
        let session := rebuildContext { session with context }
        let removed := String.intercalate ", " (indices.map contextIndexText)
        .ok (record session input s!"removed {removed}")

private def singleStatement (source : String) : Except String _root_.TPTP.Statement := do
  let document ← parseDocument source
  match document.items.toList with
  | [.statement statement] => pure statement
  | [] => .error "update expects one TPTP statement"
  | _ => .error "update expects exactly one TPTP statement"

def updateContext (session : Session) (input : String) (index : Nat) (source : String) :
    Except String Session := do
  let statement ← singleStatement source
  match session.context.find? (·.id == index) with
  | some item =>
      match item.value with
      | .include _ =>
          .error s!"context index {contextIndexText index} is an include; remove it instead"
      | .statement _ =>
          let context := session.context.map fun item =>
            if item.id == index then { item with value := .statement statement } else item
          let session := rebuildContext { session with context }
          pure (record session input s!"updated {contextIndexText index}")
  | none => .error s!"unknown context index {contextIndexText index}"

def problem (session : Session) : Option Problem :=
  if session.problemSource.isEmpty then none else some {
    name := "oatp-repl"
    source := session.problemSource
  }

end OATP.Repl
