/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.Lean.Repl
import OATP.Repl
import TermColor.ColorScheme
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
  deriving Repr

structure JobResult where
  cell : Nat
  input : String
  output : String
  ok : Bool
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

private def transcriptLine (entry : TranscriptEntry) : Text :=
  let input := Text.styled s!"[{entry.cell}] " (Style.dim <+> Style.fg theme.comment) ++
    Text.styled "› " (Style.bold <+> Style.fg theme.orange) ++
    Text.styled entry.input (Style.fg theme.foreground)
  let marker := if entry.ok then "=" else "!"
  let style := if entry.ok then Style.fg theme.green else Style.fg theme.red
  let output := match entry.output.splitOn "\n" with
    | [] => Text.empty
    | line :: rest =>
        let first := Text.styled s!"  {marker} " (Style.bold <+> style) ++ Text.plain line
        rest.foldl (fun output line => output ++ Text.plain "\n    " ++ Text.plain line)
          first
  input ++ Text.plain "\n" ++ output

private def fitEntries (_width budget : Nat) (entries : List TranscriptEntry) : List Text :=
  let rec keep (remaining : Nat) (kept : List Text) : List TranscriptEntry → List Text
    | [] => kept
    | entry :: older =>
        let view := transcriptLine entry
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

private def formulaLine (formula : FormulaView) : Text :=
  Text.plain s!"{formula.cell} {formula.role} {formula.name}: {formula.formula}"

private def symbolLine (symbol : Symbol) : Text :=
  let kind := match symbol.kind with
    | .variable => "var"
    | .constant => "const"
    | .function => "fun"
    | .predicate => "pred"
  Text.plain s!"{kind} {symbol.name}/{symbol.arity}"

private def stateSection (title : String) : Text :=
  Text.styled title (Style.bold <+> Style.fg theme.purple)

private def contextPanel (app : App) (width : Nat) : Text :=
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
  box body { title := some (Text.styled "context" (Style.bold <+> Style.fg theme.cyan))
           , borderStyle := Style.fg theme.selection, maxWidth := some width }

private def historyPanel (app : App) (width : Nat) : Text :=
  let rows := app.session.history.toList.reverse.take 18
  let body := if rows.isEmpty then
      Text.styled "No commands yet." (Style.dim <+> Style.fg theme.comment)
    else
      joinLines (rows.map fun entry =>
        Text.styled s!"[{entry.cell}] {entry.input}" (Style.fg theme.foreground) ++
          Text.plain "\n" ++
          Text.styled "  = " (Style.bold <+> Style.fg theme.green) ++
          fitText (max 1 (width - 6)) entry.result)
  box body { title := some (Text.styled "history" (Style.bold <+> Style.fg theme.cyan))
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
  let hint := if app.stateOpen then "/state close"
    else if app.historyOpen then "/history close"
    else "/state context • /history"
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
        let right := if app.historyOpen then historyPanel app rightWidth
          else contextPanel app rightWidth
        columns [leftWidth, rightWidth] 2 [left, right] []
          (Text.styled "│" (Style.fg theme.selection))
    | _, _ => calcContent app size
  opaqueScreen size content

end OATP.ReplView
