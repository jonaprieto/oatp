/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.Lean.Repl
import OATP.Repl
import TermColor.ColorScheme
import TermColor.Diagnostics
import TermColor.Repl
import TermColor.Terminal
import TermColor.Widgets

/-!
# OATP.ReplView: pure terminal rendering

All UI output is derived from `App` data. The runtime loop owns IO, while this module keeps the
transcript, context drawer, and prompt width-aware and testable without a TTY.
-/

namespace OATP.ReplView

open OATP
open OATP.Repl
open TermColor
open TermColor.Diagnostics
open TermColor.Layout
open TermColor.Repl
open TermColor.Terminal
open TermColor.Widgets
open scoped TermColor.Style

def version : String := "0.4.0"

def theme : ColorScheme where
  background := .rgb 13 24 37
  foreground := .rgb 225 241 245
  selection := .rgb 39 70 83
  comment := .rgb 116 155 165
  red := .rgb 243 112 131
  orange := .rgb 255 184 92
  yellow := .rgb 242 220 135
  green := .rgb 111 218 169
  cyan := .rgb 83 211 217
  blue := .rgb 110 168 255
  purple := .rgb 178 146 255
  pink := .rgb 245 132 203

structure TranscriptEntry where
  cell : Nat
  input : String
  output : String
  ok : Bool := true
  elapsedMs : Option Nat := none
  sources : Sources := #[]
  diagnostic : Option Diagnostic := none
  deriving Repr

structure JobResult where
  cell : Nat
  input : String
  output : String
  ok : Bool
  elapsedMs : Option Nat := none
  deriving Repr

structure App where
  session : OATP.Repl.Session := {}
  entries : List TranscriptEntry := []
  repl : Repl.State := {}
  stateOpen : Bool := false
  historyOpen : Bool := false
  running : Bool := true
  status : String := "ready"
  busy : Bool := false
  jobResult : Option JobResult := none
  goal : Option OATP.GoalSnapshot := none
  translation : Option String := none
  term : Option OATP.Lean.Repl.RenderedTerm := none
  leanRuntime : Option OATP.Lean.Repl.Runtime := none
  leanGoal : Option OATP.Lean.Repl.Goal := none

def fallbackSize : Size := { columns := 110, rows := 28 }

def inputConfig : TextInputConfig := { width := 160, maxLength := 16_384 }

def multilineConfig : MultilineConfig :=
  { text := inputConfig, lineBreak := .ctrl 'n' }

private def minFrameWidth : Nat := 34

private def frameWidth (width : Nat) : Nat := max minFrameWidth width

private def boxInnerWidth (outer : Nat) : Nat :=
  Layout.boxInnerWidth { padding := 1 } outer

private def panelWidth (width : Nat) : Nat := frameWidth width - 2

private def stateDrawerMinWidth : Nat := 70

private def stateDrawerWidths (width : Nat) : Option (Nat × Nat) :=
  let total := frameWidth width
  if total < stateDrawerMinWidth then none
  else
    let available := total - 2
    let right := max 34 (available * 2 / 5)
    some (available - right, right)

private def joinLines : List Text → Text
  | [] => Text.empty
  | line :: lines => lines.foldl (fun result next => result ++ Text.plain "\n" ++ next) line

private def fitText (width : Nat) (value : String) : Text :=
  truncate width (Text.plain value)

private def fillHeight (height : Nat) (text : Text) : Text :=
  let height := max 1 height
  let lines := (splitLines text).take height
  joinLines (lines ++ List.replicate (height - lines.length) Text.empty)

private def withBackground (text : Text) : Text :=
  { segments := text.segments.map fun segment =>
      { segment with style := Style.bg theme.background <+> segment.style } }

private def opaqueScreen (size : Size) (content : Text) : Text :=
  let width := max 1 size.columns
  let rows := max 1 size.rows
  let blank := Text.styled (String.ofList (List.replicate width ' '))
    (Style.bg theme.background)
  let lines := (splitLines (wrapLines width content)).take rows
  let lines := lines.map (fun line => withBackground (padRight width line))
  joinLines (lines ++ List.replicate (rows - lines.length) blank)

def formatElapsed (milliseconds : Nat) : String :=
  if milliseconds == 0 then "<1 ms"
  else if milliseconds < 1_000 then s!"{milliseconds} ms"
  else s!"{milliseconds / 1_000}.{milliseconds % 1_000 / 100} s"

private def diagnosticView (width : Nat) (sources : Sources) (diagnostic : Diagnostic) : Text :=
  TermColor.Diagnostics.render sources diagnostic
    { width := max 1 (frameWidth width - 2), contextLines := 0, hyperlinks := false } theme

private def transcriptLine (width : Nat) (entry : TranscriptEntry) : Text :=
  let input := Text.styled s!"[{entry.cell}] " (Style.dim <+> Style.fg theme.comment) ++
    Text.styled "› " (Style.bold <+> Style.fg theme.orange) ++
    Text.styled entry.input (Style.fg theme.foreground)
  let marker := if entry.ok then "=" else "!"
  let style := if entry.ok then Style.fg theme.green else Style.fg theme.red
  let marker := Text.styled s!"  {marker} " (Style.bold <+> style)
  let output := match entry.diagnostic with
    | some diagnostic =>
        match splitLines (diagnosticView width entry.sources diagnostic) with
        | [] => marker
        | line :: rest =>
            let first := marker ++ line
            rest.foldl (fun output line => output ++ Text.plain "\n    " ++ line) first
    | none =>
        match entry.output.splitOn "\n" with
        | [] => marker
        | line :: rest =>
            let first := marker ++ Text.plain line
            rest.foldl (fun output line => output ++ Text.plain "\n    " ++ Text.plain line)
              first
  let timing := entry.elapsedMs.map (fun milliseconds =>
      Text.styled s!"  ({formatElapsed milliseconds})"
        (Style.dim <+> Style.fg theme.comment)) |>.getD Text.empty
  input ++ Text.plain "\n" ++ output ++ timing

private def fitEntries (width budget : Nat) (entries : List TranscriptEntry) : List Text :=
  let rec keep (remaining : Nat) (kept : List Text) : List TranscriptEntry → List Text
    | [] => kept
    | entry :: older =>
        let view := transcriptLine width entry
        if view.height > remaining then
          if kept.isEmpty then
            [joinLines ((splitLines view).drop (view.height - remaining))]
          else kept
        else keep (remaining - view.height) (view :: kept) older
  keep budget [] entries

private def transcript (width budget : Nat) (entries : List TranscriptEntry) : Text :=
  let views := fitEntries width budget entries
  if views.isEmpty then
    Text.styled "Type a TPTP statement or /help." (Style.dim <+> Style.fg theme.comment)
  else joinLines views

private def identifierChar (character : Char) : Bool :=
  character.isAlpha || character.isDigit || character == '_' || character == '$' || character == '\''

private def symbolStyle (symbols : Array Symbol) (token : String) : Style :=
  if token.startsWith "$" then Style.fg theme.blue
  else match symbols.find? (fun symbol => symbol.name == token) with
  | some symbol => match symbol.kind with
      | .variable => Style.fg theme.yellow
      | .constant => Style.fg theme.green
      | .function => Style.fg theme.green
      | .predicate => Style.fg theme.cyan
  | none =>
      match token.toList.head? with
      | some character =>
          if character.isUpper then Style.fg theme.yellow else Style.fg theme.foreground
      | none => Style.fg theme.foreground

private def operatorStyle (token : String) : Style :=
  if token == "!" || token == "?" then Style.fg theme.pink
  else if token == "(" || token == ")" || token == "[" || token == "]" || token == "," ||
      token == ":" then Style.fg theme.purple
  else Style.fg theme.blue

private def semanticFormula (formula : FormulaView) : Text :=
  let flush := fun (state : Text × String) =>
    if state.2.isEmpty then state
    else (state.1 ++ Text.styled state.2 (symbolStyle formula.symbols state.2), "")
  let step := fun (state : Text × String) (character : Char) =>
    if identifierChar character then
      (state.1, state.2.push character)
    else
      let state := flush state
      let token := String.ofList [character]
      if character == '!' || character == '?' || character == '(' || character == ')' ||
          character == '[' || character == ']' || character == ',' || character == ':' ||
          character == '&' || character == '|' || character == '~' || character == '=' ||
          character == '<' || character == '>' then
        (state.1 ++ Text.styled token (operatorStyle token), "")
      else
        (state.1 ++ Text.plain token, "")
  (flush (formula.formula.toList.foldl step (Text.empty, ""))).1

private def formulaLine (formula : FormulaView) : Text :=
  Text.styled s!"{formula.cell} {formula.role} " (Style.dim <+> Style.fg theme.comment) ++
    Text.styled formula.name (Style.bold <+> Style.fg theme.cyan) ++
    Text.styled ": " (Style.dim <+> Style.fg theme.comment) ++ semanticFormula formula

private def symbolLine (symbol : Symbol) : Text :=
  let kind := match symbol.kind with
    | .variable => "var"
    | .constant => "const"
    | .function => "fun"
    | .predicate => "pred"
  Text.plain s!"{kind} {symbol.name}/{symbol.arity}"

private def stateSection (title : String) : Text :=
  Text.styled title (Style.bold <+> Style.fg theme.purple)

private def contextPanel (app : App) (width height : Nat) : Text :=
  let formulas := app.session.formulas.toList.reverse.take 8
  let symbols := app.session.symbols.toList.take 12
  let problem := if app.session.problemSource.isEmpty then
      "(no problem)"
    else
      String.intercalate "\n" (app.session.problemSource.splitOn "\n" |>.take 3)
  let goal := match app.goal with
    | none => "(no Lean goal)"
    | some goal => s!"⊢ {goal.target}"
  let term := match app.term with
    | none => "(no checked term)"
    | some term => s!"{term.term} : {term.type}"
  let translation := match app.translation with
    | none => "(no Lean → TPTP translation)"
    | some source => String.intercalate "\n" (source.splitOn "\n" |>.take 3)
  let body := joinLines <|
    [ stateSection s!"FORMULAS ({app.session.formulas.size})" ] ++
    (if formulas.isEmpty then [Text.plain "(none)"] else formulas.map formulaLine) ++
    [ stateSection s!"SYMBOLS ({app.session.symbols.size})" ] ++
    (if symbols.isEmpty then [Text.plain "(none)"] else symbols.map symbolLine) ++
    [ stateSection "PROBLEM"
    , fitText (max 1 (width - 4)) problem
    , stateSection "LEAN GOAL"
    , Text.plain goal
    , stateSection "LEAN → TPTP"
    , fitText (max 1 (width - 4)) translation
    , stateSection "CHECKED TERM"
    , Text.plain term ]
  let innerWidth := boxInnerWidth width
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) body)
  box body { title := some (Text.styled "context" (Style.bold <+> Style.fg theme.cyan))
           , borderStyle := Style.fg theme.selection, maxWidth := some width }

private def historyPanel (app : App) (width height : Nat) : Text :=
  let rows := app.session.history.toList.reverse.take 18
  let body := if rows.isEmpty then
      Text.styled "No commands yet." (Style.dim <+> Style.fg theme.comment)
    else
      joinLines (rows.map fun entry =>
        Text.styled s!"[{entry.cell}] {entry.input}" (Style.fg theme.foreground) ++
          Text.plain "\n" ++
          Text.styled "  = " (Style.bold <+> Style.fg theme.green) ++
          fitText (max 1 (width - 6)) entry.result)
  let innerWidth := boxInnerWidth width
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) body)
  box body { title := some (Text.styled "history • active" (Style.bold <+> Style.fg theme.cyan))
           , borderStyle := Style.fg theme.selection, maxWidth := some width }

private def mascot : Text :=
  joinLines [ Text.styled "  ◆  " (Style.fg theme.yellow)
            , Text.styled " /|\\ " (Style.fg theme.cyan)
            , Text.styled "◆─┼─◆" (Style.fg theme.green)
            , Text.styled " \\|/ " (Style.fg theme.blue) ]

private def banner (width : Nat) : Text :=
  let outer := frameWidth width
  let inner := boxInnerWidth outer
  let paneSpace := inner - 1
  let leftWidth := max 24 (paneSpace * 56 / 100)
  let rightWidth := max 24 (paneSpace - leftWidth)
  let left := align leftWidth .center (truncate leftWidth <|
    Text.styled "OATP REPL" (Style.bold <+> Style.fg theme.foreground) ++
      Text.plain "\n\n" ++ mascot ++ Text.plain "\n\n" ++
      Text.styled "TPTP • Lean • ATP" (Style.fg theme.comment))
  let right := align rightWidth .left (truncate rightWidth <|
    Text.styled "START HERE" (Style.bold <+> Style.fg theme.orange) ++
      Text.plain "\n" ++ Text.styled "/to-lean p => p" (Style.fg theme.cyan) ++
      Text.plain "\n/state  context drawer" ++
      Text.plain "\n/run --prover eprover" ++
      Text.plain "\n\nenter submit • ctrl-n newline" ++
      Text.plain "\n/help commands")
  box (columns [leftWidth, rightWidth] 1 [left, right] []
    (Text.styled "│" (Style.fg theme.selection)))
    { title := some (Text.styled s!" oatp repl v{version} "
        (Style.bold <+> Style.fg theme.orange))
      , titleAlignment := .left, borderStyle := Style.fg theme.orange
      , maxWidth := some outer }

private def compactHeader (width : Nat) : Text :=
  let outer := frameWidth width
  let title := s!" oatp repl v{version} "
  let used := title.length + 2
  Text.styled "──" (Style.fg theme.selection) ++
    Text.styled title (Style.bold <+> Style.fg theme.orange) ++
    Text.styled (String.ofList (List.replicate (if outer > used then outer - used else 0) '─'))
      (Style.fg theme.selection)

def prompt (width : Nat) (state : Repl.State) : Text :=
  let outer := frameWidth width
  box (Text.styled "› " (Style.bold <+> Style.fg theme.orange) ++
    TermColor.Repl.renderMultilineTextInputBody
      { width := max 1 (boxInnerWidth outer - 2), textStyle := Style.fg theme.foreground
        cursorStyle := Style.reverse } state.input true)
    { chars := { topLeft := '╭', topRight := '╮', bottomLeft := '╰', bottomRight := '╯' }
      , borderStyle := Style.fg theme.selection, maxWidth := some outer }

private def footer (app : App) (width : Nat) : Text :=
  let outer := frameWidth width
  let state := if app.busy then "[BUSY]" else "[READY]"
  let hint := if app.stateOpen then "/help /state close"
    else if app.historyOpen then "/help /history close"
    else "/help /state /history"
  let leftWidth := outer * 2 / 3
  let rightWidth := outer - leftWidth
  let left := Text.styled state (Style.bold <+> Style.fg (if app.busy then theme.yellow else theme.green)) ++
    Text.styled s!"  {app.session.formulas.size} formulas • {app.session.symbols.size} symbols"
      (Style.dim <+> Style.fg theme.comment)
  columns [leftWidth, rightWidth] 0
    [ truncate leftWidth left
    , truncate rightWidth (Text.styled hint (Style.dim <+> Style.fg theme.comment)) ] [.left, .left]

private def calcContent (app : App) (size : Size) : Text :=
  let width := frameWidth size.columns
  let head := if app.entries.isEmpty then banner width else compactHeader width
  let foot := prompt width app.repl ++ Text.plain "\n" ++ footer app width
  let used := head.height + foot.height + 2
  let budget := if size.rows > used then size.rows - used else 1
  let body := transcript width budget app.entries
  head ++ Text.plain "\n" ++ fillHeight budget body ++ Text.plain "\n" ++ foot

def screen (app : App) (size : Size) : Text :=
  let width := frameWidth size.columns
  let size := { size with columns := width }
  let drawerOpen := app.stateOpen || app.historyOpen
  let content := match drawerOpen, stateDrawerWidths width with
    | true, some (leftWidth, rightWidth) =>
        let left := calcContent app { size with columns := leftWidth }
        let right := if app.historyOpen then historyPanel app rightWidth size.rows
          else contextPanel app rightWidth size.rows
        columns [leftWidth, rightWidth] 2 [left, right] []
          (Text.styled "│" (Style.fg theme.selection))
    | _, _ => calcContent app size
  opaqueScreen size content

end OATP.ReplView
