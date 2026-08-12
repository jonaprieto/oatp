/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.Lean.Repl
import OATP.Repl
import OATP.SystemOnTPTP
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
open OATP.TPTP
open TermColor
open TermColor.Diagnostics
open TermColor.Layout
open TermColor.Repl
open TermColor.Terminal
open TermColor.Widgets
open scoped TermColor.Style

def version : String := OATP.version

def aurora : ColorScheme where
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

def themes : List (String × ColorScheme) :=
  [("aurora", aurora), ("terracotta", {
    background := .rgb 26 28 38
    foreground := .rgb 214 216 226
    selection := .rgb 74 78 96
    comment := .rgb 122 128 148
    red := .rgb 224 108 117
    orange := .rgb 217 119 87
    yellow := .rgb 229 192 123
    green := .rgb 152 195 121
    cyan := .rgb 137 205 209
    blue := .rgb 97 175 239
    purple := .rgb 198 120 221
    pink := .rgb 224 140 180
  }), ("catppuccin", ColorScheme.catppuccin), ("dracula", ColorScheme.dracula),
   ("monokai", ColorScheme.monokai)]

def defaultThemeName : String := "aurora"

/-- Semantic token colors extend the palette without putting syntax roles in ColorScheme. -/
structure SemanticColors where
  format : Color
  role : Color
  error : Color
  variableColor : Color
  constantColor : Color
  functionColor : Color
  predicateColor : Color

def semanticColors (scheme : ColorScheme) : SemanticColors where
  format := scheme.cyan
  role := scheme.purple
  error := scheme.red
  variableColor := scheme.yellow
  constantColor := scheme.green
  functionColor := scheme.green
  predicateColor := scheme.cyan

inductive AppKeyAction
  | openRun
  | focusDrawer
  | closeProvers
  | proverNext
  | proverPrevious
  | toggleProver
  | closeRun
  | runNext
  | runPrevious
  | runInspect
  | closeState
  | contextNext
  | contextPrevious
  | prepareRemoveContext
  | prepareUpdateContext
  | toggleContext
  | expandContext
  | collapseContext
  | closeHistory
  | transcriptPageUp
  | transcriptPageDown
  deriving BEq, DecidableEq, Repr

inductive AppContext
  | default
  | run
  | runInput
  | provers
  | proversInput
  | state
  | stateInput
  | history
  | historyInput

def AppContext.name : AppContext → String
  | .default => "default"
  | .run => "run"
  | .runInput => "run-input"
  | .provers => "provers"
  | .proversInput => "provers-input"
  | .state => "state"
  | .stateInput => "state-input"
  | .history => "history"
  | .historyInput => "history-input"

def AppContext.keyContext : AppContext → KeyContext
  | .default => KeyContext.ofString "default"
  | .run => KeyContext.ofString "run"
  | .runInput => KeyContext.ofString "run-input"
  | .provers => KeyContext.ofString "provers"
  | .proversInput => KeyContext.ofString "provers-input"
  | .state => KeyContext.ofString "state"
  | .stateInput => KeyContext.ofString "state-input"
  | .history => KeyContext.ofString "history"
  | .historyInput => KeyContext.ofString "history-input"

private def appBinding (keys : List Key) (action : AppKeyAction)
    (context : Option AppContext) (description : String) : BindingSpec AppKeyAction :=
  { keys
    action
    context := context.map AppContext.keyContext
    label := String.intercalate "/" (keys.map Keymap.keyLabel)
    description }

def appBindings : List (BindingSpec AppKeyAction) :=
  [ appBinding [.ctrl 'R', .ctrl 'r'] .openRun none "open the run drawer"
  , appBinding [.ctrl ']'] .focusDrawer none "focus the open drawer"
  , appBinding [.char 'H', .char 'h', .escape] .closeRun (some .runInput) "return to input"
  , appBinding [.char 'J', .char 'j', .down] .runNext (some .run) "next run"
  , appBinding [.char 'K', .char 'k', .up] .runPrevious (some .run) "previous run"
  , appBinding [.enter, .char ' '] .runInspect (some .run) "inspect the selected run"
  , appBinding [.char 'H', .char 'h', .escape] .closeProvers (some .proversInput) "return to input"
  , appBinding [.char 'J', .char 'j', .down] .proverNext (some .provers) "next prover"
  , appBinding [.char 'K', .char 'k', .up] .proverPrevious (some .provers) "previous prover"
  , appBinding [.enter, .char ' '] .toggleProver (some .provers) "toggle the selected prover"
  , appBinding [.char 'H', .char 'h', .escape] .closeState (some .stateInput) "return to input"
  , appBinding [.delete] .prepareRemoveContext (some .state) "prepare removal of selected item"
  , appBinding [.char 'e', .char 'E'] .prepareUpdateContext (some .state) "edit selected item"
  , appBinding [.char 'J', .char 'j', .down] .contextNext (some .state) "next context section"
  , appBinding [.char 'K', .char 'k', .up] .contextPrevious (some .state) "previous context section"
  , appBinding [.enter, .char ' '] .toggleContext (some .state) "toggle the selected section"
  , appBinding [.right] .expandContext (some .state) "expand the selected section"
  , appBinding [.left] .collapseContext (some .state) "collapse the selected section"
  , appBinding [.char 'H', .char 'h', .escape] .closeHistory (some .historyInput) "return to input"
  , appBinding [.pageUp] .transcriptPageUp (some .default) "scroll transcript up"
  , appBinding [.pageDown] .transcriptPageDown (some .default) "scroll transcript down" ]

def appKeyLabel (action : AppKeyAction) (context : AppContext) : String :=
  (appBindings.find? (fun binding => binding.action == action &&
    (binding.context == none || binding.context == some (AppContext.keyContext context)))).map
      (·.label) |>.getD "?"

def themeByName (name : String) : Option ColorScheme :=
  themes.find? (fun pair => pair.1 == name) |>.map Prod.snd

def themeNames : String := String.intercalate ", " (themes.map Prod.fst)

structure TranscriptEntry where
  cell : Nat
  input : String
  output : String
  ok : Bool := true
  elapsedMs : Option Nat := none
  sources : Sources := #[]
  diagnostic : Option Diagnostic := none
  report : Option Report := none
  reportExpanded : Bool := false
  deriving Repr

structure JobResult where
  cell : Nat
  input : String
  output : String
  ok : Bool
  elapsedMs : Option Nat := none
  report : Option Report := none
  deriving Repr

inductive RunStatus where
  | queued
  | running
  | result (status : SZSStatus)
  | failed
  | cancelled
  deriving BEq, DecidableEq, Repr

namespace RunStatus

def label : RunStatus → String
  | .queued => "queued"
  | .running => "running"
  | .result status => s!"{status}"
  | .failed => "failed"
  | .cancelled => "cancelled"

end RunStatus

structure RunRow where
  name : String
  status : RunStatus := .queued
  detail : String := ""
  elapsedMs : Option Nat := none
  deriving Repr

inductive PanelFocus where
  | main
  | drawer
  deriving BEq, DecidableEq, Repr

inductive ContextTarget where
  | formulas
  | symbols
  | problem
  | goal
  | translation
  | term
  deriving BEq, DecidableEq, Repr

namespace ContextTarget

def all : List ContextTarget :=
  [.formulas, .symbols, .problem, .goal, .translation, .term]

def name : ContextTarget → String
  | .formulas => "formulas"
  | .symbols => "symbols"
  | .problem => "problem"
  | .goal => "goal"
  | .translation => "translation"
  | .term => "term"

def label : ContextTarget → String
  | .formulas => "FORMULAS"
  | .symbols => "SYMBOLS"
  | .problem => "PROBLEM"
  | .goal => "LEAN GOAL"
  | .translation => "LEAN → TPTP"
  | .term => "CHECKED TERM"

def aliases : ContextTarget → List String
  | .formulas => ["form", "formula", "formulas"]
  | .symbols => ["symbol", "symbols"]
  | .problem => ["problem"]
  | .goal => ["goal", "lean"]
  | .translation => ["translation", "tptp"]
  | .term => ["term", "checked-term", "checked"]

private def indexOf (value : String) : List ContextTarget → Nat → Option Nat
  | [], _ => none
  | contextTarget :: rest, index =>
      if (aliases contextTarget).any (· == value) then some index
      else indexOf value rest (index + 1)

def indexOfString (value : String) : Option Nat := indexOf value.toLower all 0

end ContextTarget

def contextSectionCount : Nat := ContextTarget.all.length

structure App where
  session : OATP.Repl.Session := {}
  entries : List TranscriptEntry := []
  transcriptScroll : Nat := 0
  repl : Repl.State := {}
  stateOpen : Bool := true
  historyOpen : Bool := false
  proversOpen : Bool := false
  runOpen : Bool := false
  panelFocus : PanelFocus := .main
  proverFocus : Nat := 0
  proverChoices : Array ProverReference := #[]
  runFocus : Nat := 0
  runRows : Array RunRow := #[]
  runFrame : Nat := 0
  runProgress : Option (IO.Ref (Array RunRow)) := none
  selectionStart : Option (Nat × Nat) := none
  selectionEnd : Option (Nat × Nat) := none
  copyPending : Option String := none
  contextFocus : Nat := 0
  contextItemFocus : Option Nat := none
  contextExpanded : Array Bool := Array.replicate contextSectionCount true
  running : Bool := true
  statusNotice : Option String := none
  theme : ColorScheme := aurora
  themeName : String := defaultThemeName
  theory : String := OATP.TPTP.defaultTheory
  defaultProver : Option ProverReference := none
  enabledProvers : Array ProverReference := #[]
  proverSelectionSet : Bool := false
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

def drawerWidths (width : Nat) : Option (Nat × Nat) :=
  stateDrawerWidths width

private def joinLines : List Text → Text
  | [] => Text.empty
  | line :: lines => lines.foldl (fun result next => result ++ Text.plain "\n" ++ next) line

private def fitText (width : Nat) (value : String) : Text :=
  truncate width (Text.plain value)

private def fillHeight (height : Nat) (text : Text) : Text :=
  let height := max 1 height
  let lines := (splitLines text).take height
  joinLines (lines ++ List.replicate (height - lines.length) Text.empty)

private def withBackground (scheme : ColorScheme) (text : Text) : Text :=
  { segments := text.segments.map fun segment =>
      { segment with style := Style.bg scheme.background <+> segment.style } }

private def dimText (text : Text) : Text :=
  { segments := text.segments.map fun segment =>
      { segment with style := Style.dim <+> segment.style } }

private def base64Alphabet : String :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

private def base64Char (index : Nat) : Char :=
  base64Alphabet.toList.getD index 'A'

private def base64 : List UInt8 → String
  | a :: b :: c :: rest =>
      let value := a.toNat * 65_536 + b.toNat * 256 + c.toNat
      String.ofList [base64Char (value / 262_144), base64Char ((value / 4_096) % 64),
        base64Char ((value / 64) % 64), base64Char (value % 64)] ++ base64 rest
  | [a, b] =>
      let value := a.toNat * 256 + b.toNat
      String.ofList [base64Char (value / 1_024), base64Char ((value / 16) % 64),
        base64Char ((value % 16) * 4), '=']
  | [a] =>
      let value := a.toNat
      String.ofList [base64Char (value / 4), base64Char ((value % 4) * 16), '=', '=']
  | [] => ""

private def clipboardSequence (value : String) : String :=
  "\u001b]52;c;" ++ base64 value.toUTF8.toList ++ "\u0007"

private def opaqueScreen (scheme : ColorScheme) (size : Size) (content : Text)
    (copyPending : Option String := none) : Text :=
  let width := max 1 size.columns
  let rows := max 1 size.rows
  let blank := Text.styled (String.ofList (List.replicate width ' '))
    (Style.bg scheme.background)
  let lines := (splitLines (wrapLines width content)).take rows
  let lines := lines.map (fun line => withBackground scheme (padRight width line))
  let screen := joinLines (lines ++ List.replicate (rows - lines.length) blank)
  match copyPending with
  | some value => screen ++ Text.plain (clipboardSequence value)
  | none => screen

def formatElapsed (milliseconds : Nat) : String :=
  if milliseconds == 0 then "<1 ms"
  else if milliseconds < 1_000 then s!"{milliseconds} ms"
  else s!"{milliseconds / 1_000}.{milliseconds % 1_000 / 100} s"

private def diagnosticView (scheme : ColorScheme) (width : Nat) (sources : Sources)
    (diagnostic : Diagnostic) : Text :=
  TermColor.Diagnostics.render sources diagnostic
    { width := max 1 (frameWidth width - 2), contextLines := 0, hyperlinks := false } scheme

private def identifierChar (character : Char) : Bool :=
  character.isAlpha || character.isDigit || character == '_' || character == '$' ||
    character == '\''

private def roleStyle (semantic : SemanticColors) : _root_.TPTP.Role → Option Style
  | .axiom | .hypothesis | .definition | .assumption | .lemma | .theorem | .corollary
    | .conjecture | .negatedConjecture | .plain | .type | .interpretation | .logic
    | .unknown | .finiteDomain | .finiteFunctor | .finitePredicate =>
      some (Style.fg semantic.role)
  | .other _ => none

private def symbolStyle (scheme : ColorScheme) (symbols : Array Symbol) (token : String)
    (highlightSyntax : Bool := true) : Style :=
  let lower := token.toLower
  let semantic := semanticColors scheme
  if token.startsWith "/" then Style.bold <+> Style.fg scheme.orange
  else if token.startsWith "--" then Style.fg scheme.blue
  else if token == "-" then Style.fg scheme.blue
  else if token.startsWith "$" then Style.fg scheme.blue
  else if !highlightSyntax then Style.fg scheme.foreground
  else match _root_.TPTP.Kind.ofString lower with
  | .fof | .cnf | .tff | .thf | .tcf | .tpi => Style.bold <+> Style.fg semantic.format
  | .other _ =>
      match roleStyle semantic (_root_.TPTP.Role.ofString lower) with
      | some style => style
      | none =>
          match lower with
          | "error" | "failed" => Style.bold <+> Style.fg semantic.error
          | _ => match symbols.find? (fun symbol => symbol.name == token) with
              | some symbol => match symbol.kind with
                  | .variable => Style.fg semantic.variableColor
                  | .constant => Style.fg semantic.constantColor
                  | .function => Style.fg semantic.functionColor
                  | .predicate => Style.fg semantic.predicateColor
              | none => Style.fg scheme.foreground

private def operatorStyle (scheme : ColorScheme) (token : String) : Style :=
  if token == "!" || token == "?" then Style.fg scheme.pink
  else if token == "(" || token == ")" || token == "[" || token == "]" || token == "," ||
      token == ":" || token == "." || token == ";" then Style.fg scheme.purple
  else Style.fg scheme.blue

private def semanticText (scheme : ColorScheme) (symbols : Array Symbol) (value : String)
    (highlightSyntax : Bool := true) : Text :=
  let flush := fun (state : Text × String) =>
    if state.2.isEmpty then state
    else (state.1 ++ Text.styled state.2
      (symbolStyle scheme symbols state.2 highlightSyntax), "")
  let step := fun (state : Text × String) (character : Char) =>
    if identifierChar character || character == '/' || character == '-' then
      (state.1, state.2.push character)
    else
      let state := flush state
      let token := String.ofList [character]
      if character == '!' || character == '?' || character == '(' || character == ')' ||
          character == '[' || character == ']' || character == ',' || character == ':' ||
          character == '&' || character == '|' || character == '~' || character == '=' ||
          character == '<' || character == '>' || character == '.' || character == ';' ||
          character == '⊢' || character == '→' then
        (state.1 ++ Text.styled token
          (if highlightSyntax then operatorStyle scheme token else Style.fg scheme.foreground), "")
      else
        (state.1 ++ Text.plain token, "")
  (flush (value.toList.foldl step (Text.empty, ""))).1

private def semanticFormula (scheme : ColorScheme) (formula : FormulaView) : Text :=
  semanticText scheme formula.symbols formula.formula

private def formulaLine (scheme : ColorScheme) (selected : Option Nat)
    (formula : FormulaView) : Text :=
  let marker := if selected == some formula.id then "▸ " else "  "
  Text.styled s!"{marker}#{formula.id} [{formula.cell}] {formula.role} "
      (Style.dim <+> Style.fg scheme.comment) ++
    Text.styled formula.name (Style.bold <+> Style.fg scheme.cyan) ++
    Text.styled ": " (Style.dim <+> Style.fg scheme.comment) ++ semanticFormula scheme formula

private def reportWidgetConfig (scheme : ColorScheme) : CollapsibleConfig := {
  collapsedMarker := Text.styled "▸ " (Style.fg scheme.comment)
  expandedMarker := Text.styled "▾ " (Style.fg scheme.orange)
  summaryStyle := Style.bold <+> Style.fg scheme.purple
  bodyStyle := Style.fg scheme.foreground
  bodyPrefix := Text.plain "    "
  maxBodyLines := 10
  overflowText := Text.styled "… more" (Style.dim <+> Style.fg scheme.comment)
  emptyText := Text.styled "(no report details)" (Style.dim <+> Style.fg scheme.comment) }

private def reportSummary (scheme : ColorScheme) (report : Report) : Text :=
  let marker := match report.severity with
    | .error => Text.styled "issues" (Style.bold <+> Style.fg scheme.red)
    | .warning => Text.styled "warning" (Style.bold <+> Style.fg scheme.yellow)
    | _ => Text.styled "check" (Style.bold <+> Style.fg scheme.green)
  marker ++ Text.plain s!" • {report.title} • click to expand • Ctrl-R full output"

private def reportBody (scheme : ColorScheme) (symbols : Array Symbol) (width : Nat)
    (entry : TranscriptEntry) (report : Report) : Text :=
  renderReport report { width := max 1 (width - 6) } scheme ++ Text.plain "\n" ++
    semanticText scheme symbols entry.output false

private def transcriptLine (scheme : ColorScheme) (symbols : Array Symbol) (width : Nat)
    (entry : TranscriptEntry) : Text :=
  let input := Text.styled s!"[{entry.cell}] " (Style.dim <+> Style.fg scheme.comment) ++
    Text.styled "› " (Style.bold <+> Style.fg scheme.orange) ++
    semanticText scheme symbols entry.input false
  let marker := if entry.ok then "=" else "!"
  let style := if entry.ok then Style.fg scheme.green else Style.fg scheme.red
  let marker := Text.styled s!"  {marker} " (Style.bold <+> style)
  let output := match entry.report, entry.diagnostic with
    | some report, _ =>
        let widget := renderCollapsible (reportWidgetConfig scheme) (max 1 (width - 2))
          (reportSummary scheme report) (reportBody scheme symbols width entry report)
          { expanded := entry.reportExpanded }
        marker ++ Text.plain " " ++ widget.text
    | none, some diagnostic =>
        match splitLines (diagnosticView scheme width entry.sources diagnostic) with
        | [] => marker
        | line :: rest =>
            let first := marker ++ line
            rest.foldl (fun output line => output ++ Text.plain "\n    " ++ line) first
    | none, none =>
        match entry.output.splitOn "\n" with
        | [] => marker
        | line :: rest =>
            let first := marker ++ semanticText scheme symbols line false
            rest.foldl (fun output line => output ++ Text.plain "\n    " ++
              semanticText scheme symbols line false)
              first
  let timing := entry.elapsedMs.map (fun milliseconds =>
      Text.styled s!"  ({formatElapsed milliseconds})"
        (Style.dim <+> Style.fg scheme.comment)) |>.getD Text.empty
  input ++ Text.plain "\n" ++ output ++ timing

private def spacedEntries : List Text → List Text
  | [] => []
  | [entry] => [entry]
  | entry :: rest => entry :: Text.empty :: spacedEntries rest

private def transcriptLines (scheme : ColorScheme) (symbols : Array Symbol) (width : Nat)
    (entries : List TranscriptEntry) : List Text :=
  let views := entries.reverse.map (transcriptLine scheme symbols width)
  (spacedEntries views).flatMap splitLines

private def reportWidget (scheme : ColorScheme) (symbols : Array Symbol) (width : Nat)
    (entry : TranscriptEntry) : Option CollapsibleRender := do
  let report ← entry.report
  pure <| renderCollapsible (reportWidgetConfig scheme) (max 1 (width - 2))
    (reportSummary scheme report) (reportBody scheme symbols width entry report)
    { expanded := entry.reportExpanded }

def reportAtTranscriptRow (app : App) (width row : Nat) : Option Nat :=
  let rec find : List TranscriptEntry → Nat → Option Nat
    | [], _ => none
    | entry :: rest, offset =>
        let text := transcriptLine app.theme app.session.symbols width entry
        let lineCount := text.height
        match entry.report, reportWidget app.theme app.session.symbols width entry with
        | some _, some widget =>
            let headerStart := offset + 1
            if headerStart ≤ row && row < headerStart + widget.hitHeaderHeight then
              some entry.cell
            else find rest (offset + lineCount + 1)
        | _, _ => find rest (offset + lineCount + 1)
  find app.entries.reverse 0

def toggleReportCell (app : App) (cell : Nat) : App :=
  { app with
    entries := app.entries.map fun entry =>
      if entry.cell == cell then { entry with reportExpanded := !entry.reportExpanded } else entry }

private def visibleTranscriptLines (app : App) (width budget : Nat) : List Text :=
  let lines := transcriptLines app.theme app.session.symbols width app.entries
  let scroll := min app.transcriptScroll (lines.length - budget)
  let visibleEnd := lines.length - scroll
  let start := visibleEnd - min budget visibleEnd
  (lines.drop start).take (visibleEnd - start)

private def transcript (app : App) (width budget bodyStart : Nat) : Text :=
  let lines := visibleTranscriptLines app width budget
  if lines.isEmpty then
    Text.styled "Type a TPTP statement or /help." (Style.dim <+> Style.fg app.theme.comment)
  else
    let body := joinLines lines
    match app.selectionStart, app.selectionEnd with
    | some (_, start), some (_, finish) =>
        let low := min start finish
        let high := max start finish
        let low := if low > bodyStart then low - bodyStart else 0
        let high := if high > bodyStart then high - bodyStart else 0
        joinLines <| (splitLines body).mapIdx fun index line =>
          if low ≤ index && index ≤ high then
            Text.styled line.plainText (Style.bg app.theme.selection)
          else line
    | _, _ => body

private def selectionLines (app : App) (width budget : Nat) : List String :=
  (visibleTranscriptLines app width budget).map (·.plainText)

def scrollTranscriptUp (app : App) : App :=
  { app with transcriptScroll := app.transcriptScroll + 3 }

def scrollTranscriptDown (app : App) : App :=
  { app with transcriptScroll := app.transcriptScroll - min 3 app.transcriptScroll }

def scrollTranscriptPageUp (app : App) : App :=
  { app with transcriptScroll := app.transcriptScroll + 10 }

def scrollTranscriptPageDown (app : App) : App :=
  { app with transcriptScroll := app.transcriptScroll - min 10 app.transcriptScroll }

def clearSelection (app : App) : App :=
  { app with selectionStart := none, selectionEnd := none }

private def symbolLine (scheme : ColorScheme) (symbol : Symbol) : Text :=
  let kind := match symbol.kind with
    | .variable => "var"
    | .constant => "const"
    | .function => "fun"
    | .predicate => "pred"
  let kindStyle := Style.dim <+> Style.fg scheme.comment
  Text.styled kind kindStyle ++ Text.plain " " ++
    Text.styled symbol.name (symbolStyle scheme #[symbol] symbol.name) ++
    Text.styled s!"/{symbol.arity}" (Style.dim <+> Style.fg scheme.comment)

private def contextWidgetConfig (scheme : ColorScheme) : CollapsibleConfig where
  collapsedMarker := Text.styled "▸ " (Style.fg scheme.comment)
  expandedMarker := Text.styled "▾ " (Style.fg scheme.orange)
  summaryStyle := Style.bold <+> Style.fg scheme.purple
  bodyStyle := Style.fg scheme.foreground
  focusStyle := Style.reverse
  bodyPrefix := Text.plain "  "
  maxBodyLines := 8
  overflowText := Text.styled "… more" (Style.dim <+> Style.fg scheme.comment)
  emptyText := Text.styled "(none)" (Style.dim <+> Style.fg scheme.comment)

private def contextSections (app : App) : List (Text × Text) :=
  let formulas := app.session.formulas.toList.reverse.take 8
  let symbols := app.session.symbols.toList.take 12
  let problem := if app.session.context.isEmpty then
      "(no problem)"
    else
      String.intercalate "\n" (app.session.context.toList.take 3 |>.map fun item =>
        s!"#{item.id} {item.value.render}")
  let goal := match app.goal with
    | none => Text.plain "(no Lean goal)"
    | some goal => joinLines <| goal.context.toList.map
        (semanticText app.theme app.session.symbols) ++
        [semanticText app.theme app.session.symbols s!"⊢ {goal.target}"]
  let term := match app.term with
    | none => Text.plain "(no checked term)"
    | some term => semanticText app.theme app.session.symbols s!"{term.term} : {term.type}"
  let translation := match app.translation with
    | none => "(no Lean → TPTP translation)"
    | some source => String.intercalate "\n" (source.splitOn "\n" |>.take 3)
  let render : ContextTarget → Text × Text
    | .formulas => (Text.plain s!"{ContextTarget.label .formulas} ({app.session.formulas.size})",
        joinLines (if formulas.isEmpty then [] else
          formulas.map (formulaLine app.theme app.contextItemFocus)))
    | .symbols => (Text.plain s!"{ContextTarget.label .symbols} ({app.session.symbols.size})",
        joinLines (symbols.map (symbolLine app.theme)))
    | .problem => (Text.plain (ContextTarget.label .problem),
        semanticText app.theme app.session.symbols problem)
    | .goal => (Text.plain (ContextTarget.label .goal), goal)
    | .translation => (Text.plain (ContextTarget.label .translation),
        semanticText app.theme app.session.symbols translation)
    | .term => (Text.plain (ContextTarget.label .term), term)
  ContextTarget.all.map render

private def contextExpandedAt (app : App) (index : Nat) : Bool :=
  app.contextExpanded.getD index true

private def contextState (app : App) (index : Nat) : CollapsibleState :=
  { expanded := contextExpandedAt app index
    focused := app.contextFocus == index }

private def contextRenders (app : App) (width : Nat) : List CollapsibleRender :=
  let innerWidth := max 1 (boxInnerWidth width)
  let rec go : List (Text × Text) → Nat → List CollapsibleRender
    | [], _ => []
    | (summary, body) :: sections, index =>
        renderCollapsible (contextWidgetConfig app.theme) innerWidth summary body
          (contextState app index) ::
          go sections (index + 1)
  go (contextSections app) 0

private def contextTexts (app : App) (width : Nat) : List Text :=
  (contextRenders app width).map (fun render => render.text)

def contextHitAtRow (app : App) (width row : Nat) : Option (Nat × Bool) :=
  if row < 2 then none
  else
    let relativeRow := row - 2
    let rec find : List CollapsibleRender → Nat → Nat → Option (Nat × Bool)
      | [], _, _ => none
      | widget :: renders, index, offset =>
          if relativeRow < offset + widget.lineCount then
            some (index, relativeRow - offset < widget.hitHeaderHeight)
          else find renders (index + 1) (offset + widget.lineCount)
    find (contextRenders app width) 0 0

def contextFormulaAtRow (app : App) (width row : Nat) : Option Nat :=
  match contextHitAtRow app width row with
  | some (0, false) =>
      let relativeRow := row - 2
      match contextRenders app width with
      | widget :: _ =>
          let bodyRow := relativeRow - widget.hitHeaderHeight
          let formulas := app.session.formulas.toList.reverse.take 8
          formulas[bodyRow]?.map (·.id)
      | [] => none
  | _ => none

private def updateContextExpanded (app : App) (index : Nat) (expanded : Bool) : App :=
  { app with contextExpanded := app.contextExpanded.set! index expanded }

def focusContext (app : App) (index : Nat) : App :=
  { app with contextFocus := min (contextSectionCount - 1) index }

def focusNextContext (app : App) : App :=
  focusContext app ((app.contextFocus + 1) % contextSectionCount)

def focusPreviousContext (app : App) : App :=
  focusContext app (if app.contextFocus == 0 then contextSectionCount - 1 else app.contextFocus - 1)

private def formulaIds (app : App) : List Nat :=
  app.session.formulas.toList.reverse.map (·.id)

private def indexOfFormula : Nat → Nat → List Nat → Option Nat
  | _, _, [] => none
  | wanted, index, id :: ids =>
      if wanted == id then some index else indexOfFormula wanted (index + 1) ids

private def nextFormulaId (app : App) (forward : Bool) : Option Nat :=
  let ids := formulaIds app
  match ids with
  | [] => none
  | first :: _ =>
      match app.contextItemFocus with
      | none => some first
      | some current =>
          let index := (indexOfFormula current 0 ids).getD 0
          if forward then ids[(index + 1) % ids.length]?
          else ids[(index + ids.length - 1) % ids.length]?

def focusContextItem (app : App) (id : Nat) : App :=
  if app.session.formulas.any (·.id == id) then
    { app with contextFocus := 0, contextItemFocus := some id }
  else app

def focusNextContextItem (app : App) : App :=
  match nextFormulaId app true with
  | some id => focusContextItem app id
  | none => { app with contextItemFocus := none }

def focusPreviousContextItem (app : App) : App :=
  match nextFormulaId app false with
  | some id => focusContextItem app id
  | none => { app with contextItemFocus := none }

def focusNextContextEntry (app : App) : App :=
  if app.contextFocus == 0 && app.session.formulas.isEmpty == false then
    focusNextContextItem app
  else focusNextContext app

def focusPreviousContextEntry (app : App) : App :=
  if app.contextFocus == 0 && app.session.formulas.isEmpty == false then
    focusPreviousContextItem app
  else focusPreviousContext app

def toggleFocusedContext (app : App) : App :=
  updateContextExpanded app app.contextFocus (!contextExpandedAt app app.contextFocus)

def expandFocusedContext (app : App) : App :=
  updateContextExpanded app app.contextFocus true

def collapseFocusedContext (app : App) : App :=
  updateContextExpanded app app.contextFocus false

def focusProver (app : App) (index : Nat) : App :=
  let focus := if app.proverChoices.isEmpty then 0 else min (app.proverChoices.size - 1) index
  { app with proverFocus := focus }

def focusNextProver (app : App) : App :=
  focusProver app ((app.proverFocus + 1) % max 1 app.proverChoices.size)

def focusPreviousProver (app : App) : App :=
  focusProver app (if app.proverFocus == 0 then max 1 app.proverChoices.size - 1
    else app.proverFocus - 1)

def focusRun (app : App) (index : Nat) : App :=
  let focus := if app.runRows.isEmpty then 0 else min (app.runRows.size - 1) index
  { app with runFocus := focus }

def focusNextRun (app : App) : App :=
  focusRun app ((app.runFocus + 1) % max 1 app.runRows.size)

def focusPreviousRun (app : App) : App :=
  focusRun app (if app.runFocus == 0 then max 1 app.runRows.size - 1 else app.runFocus - 1)

def toggleFocusedProver (app : App) : App :=
  if app.proverChoices.isEmpty then app else
    let reference := app.proverChoices[app.proverFocus]!
    let enabled := app.enabledProvers.any (· == reference)
    let updated := { app with enabledProvers := if enabled then
        app.enabledProvers.filter (· != reference)
      else app.enabledProvers.push reference }
    let updated := { updated with proverSelectionSet := true }
    { updated with statusNotice := some s!"{ProverReference.display reference} {
      if enabled then "disabled" else "enabled"}" }

def contextTargetNames : List String :=
  ContextTarget.all.map ContextTarget.name

def contextTargetOfString (value : String) : Option Nat :=
  ContextTarget.indexOfString value

private def prepareContextCommand (app : App) (command : String) : App :=
  match app.contextItemFocus with
  | none => { app with statusNotice := some "select a formula in the FORMULAS box first" }
  | some id =>
      if !app.repl.input.value.isEmpty then
        { app with statusNotice := some "clear the input before preparing a context edit" }
      else
        let value := s!"/{command} {id}" ++ if command == "update" then " " else ""
        { app with
          repl := { app.repl with
            input := { value, cursor := value.toList.length }
            historyIndex := none
            completion := none }
          stateOpen := false
          panelFocus := .main
          statusNotice := none }

def removeContextItem (app : App) : App := prepareContextCommand app "remove"

def editContextItem (app : App) : App := prepareContextCommand app "update"

def openContextTarget (app : App) (target : String) : Option App :=
  if target.toLower == "all" then
    let app := { app with stateOpen := true }
    some { app with contextExpanded := Array.replicate contextSectionCount true }
  else
    match contextTargetOfString target with
    | some index =>
        let app := expandFocusedContext <| focusContext { app with stateOpen := true } index
        some (if index == 0 then focusNextContextItem app else app)
    | none => none

private def contextPanel (app : App) (width height : Nat) : Text :=
  let body := joinLines (contextTexts app width)
  let innerWidth := boxInnerWidth width
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) body)
  let active := app.panelFocus == .drawer
  let title := if active then "state • active • H main"
    else "state • inactive • Ctrl-] focus"
  box body { title := some (Text.styled title (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg (if active then app.theme.selection else app.theme.comment)
           , maxWidth := some width }

private def historyPanel (app : App) (width height : Nat) : Text :=
  let rows := app.session.history.toList.reverse.take 18
  let body := if rows.isEmpty then
      Text.styled "No commands yet." (Style.dim <+> Style.fg app.theme.comment)
    else
      joinLines (rows.map fun entry =>
        Text.styled s!"[{entry.cell}] " (Style.dim <+> Style.fg app.theme.comment) ++
          semanticText app.theme app.session.symbols entry.input false ++
          Text.plain "\n" ++
          Text.styled "  = " (Style.bold <+> Style.fg app.theme.green) ++
          semanticText app.theme app.session.symbols
            (fitText (max 1 (width - 6)) entry.result).plainText false)
  let innerWidth := boxInnerWidth width
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) body)
  box body { title := some (Text.styled "history • active"
      (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg app.theme.selection, maxWidth := some width }

def proverVisibleStart (app : App) (height : Nat) : Nat :=
  let visible := max 1 (height - 2)
  if app.proverFocus >= visible then app.proverFocus - visible + 1 else 0

private def proverPanel (app : App) (width height : Nat) : Text :=
  let visible := max 1 (height - 2)
  let start := proverVisibleStart app height
  let rows := if app.proverChoices.isEmpty then
      [Text.styled "No local or online provers." (Style.dim <+> Style.fg app.theme.comment)]
    else app.proverChoices.toList.drop start |>.take visible |>.mapIdx fun offset reference =>
      let index := start + offset
      let checked := app.enabledProvers.any (· == reference)
      let marker := if checked then "[x]" else "[ ]"
      let style := if index == app.proverFocus then Style.reverse else {}
      let nameStyle := match reference.kind with
        | .online => Style.fg app.theme.purple
        | .local => {}
      Text.styled s!"{marker} " style ++
        Text.styled (ProverReference.display reference) (style <+> nameStyle)
  let body := padRight (boxInnerWidth width) (fillHeight (max 1 (height - 2)) (joinLines rows))
  let active := app.panelFocus == .drawer
  let title := if active then "provers • active • H main"
    else "provers • inactive • Ctrl-] focus"
  box body { title := some (Text.styled title (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg (if active then app.theme.selection else app.theme.comment)
           , maxWidth := some width }

private def runStatusStyle (scheme : ColorScheme) : RunStatus → Style
  | .result .theorem | .result .unsatisfiable => Style.bold <+> Style.fg scheme.green
  | .running | .queued => Style.fg scheme.yellow
  | .result .error | .failed => Style.bold <+> Style.fg scheme.red
  | _ => Style.fg scheme.comment

private def runPanel (app : App) (width height : Nat) : Text :=
  let innerWidth := boxInnerWidth width
  let rows := if app.runRows.isEmpty then
      [Text.styled "No active prover run." (Style.dim <+> Style.fg app.theme.comment)]
    else app.runRows.toList.mapIdx fun index row =>
      let focused := app.runFocus == index && app.panelFocus == .drawer
      let status := if row.status == .running then
          shimmer { base := app.theme.comment, highlight := app.theme.foreground, band := 4 }
            { frame := app.runFrame } (Text.plain "running")
        else Text.styled row.status.label (runStatusStyle app.theme row.status)
      let elapsed := row.elapsedMs.map (fun value => s!" {value}ms") |>.getD ""
      let marker := if focused then "› " else "  "
      let line := Text.plain marker ++
        Text.styled row.name (if focused then Style.reverse else {}) ++
        Text.plain "  " ++ status ++ Text.plain elapsed
      if row.detail.isEmpty || !focused then line
      else
        let details := splitLines (wrapLines (max 1 (innerWidth - 2)) (Text.plain row.detail))
        line ++ Text.plain "\n  " ++ joinLines details
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) (joinLines rows))
  let active := app.panelFocus == .drawer
  let title := if active then "run • active • H main" else "run • inactive • Ctrl-R open"
  box body { title := some (Text.styled title (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg (if active then app.theme.selection else app.theme.comment)
           , maxWidth := some width }

private def mascot (scheme : ColorScheme) : Text :=
  let arrow := Text.styled "──▶" (Style.fg scheme.cyan)
  joinLines [ Text.styled "TPTP" (Style.fg scheme.yellow) ++ Text.plain " " ++ arrow ++
                Text.plain " " ++ Text.styled "OATP" (Style.bold <+> Style.fg scheme.green) ++
                Text.plain " " ++ arrow ++ Text.styled " ATPs" (Style.fg scheme.purple)
            , Text.plain "          " ++ Text.styled "│" (Style.fg scheme.cyan)
            , Text.plain "          " ++ Text.styled "▼" (Style.fg scheme.cyan)
            , Text.plain "         " ++ Text.styled "LEAN ✓" (Style.fg scheme.blue) ]

private def banner (scheme : ColorScheme) (width : Nat) : Text :=
  let outer := frameWidth width
  let inner := boxInnerWidth outer
  let leftContent :=
    Text.styled "OATP REPL" (Style.bold <+> Style.fg scheme.foreground) ++
      Text.plain "\n" ++ Text.styled "ORCHESTRATED ATP" (Style.dim <+> Style.fg scheme.comment) ++
      Text.plain "\n\n" ++ mascot scheme ++ Text.plain "\n\n" ++
      Text.styled "INPUT • ORCHESTRATE • CHECK" (Style.fg scheme.comment)
  let compactRight :=
    Text.styled "/check  /run  /help" (Style.fg scheme.cyan) ++
      Text.plain "\n" ++
      Text.styled "check default • run portfolio" (Style.dim <+> Style.fg scheme.comment)
  let compactLeft :=
    Text.styled "OATP REPL" (Style.bold <+> Style.fg scheme.foreground) ++
      Text.plain "\n\n" ++ mascot scheme ++ Text.plain "\n\n" ++
      Text.styled "INPUT • ORCHESTRATE • CHECK" (Style.fg scheme.comment)
  let rightContent :=
    Text.styled "START HERE" (Style.bold <+> Style.fg scheme.orange) ++
      Text.plain "\n" ++ Text.styled "/to-lean p => p" (Style.fg scheme.cyan) ++
        Text.styled "  create a Lean goal" (Style.dim <+> Style.fg scheme.comment) ++
      Text.plain "\n" ++ Text.styled "/state" (Style.fg scheme.cyan) ++
        Text.styled "           inspect context" (Style.dim <+> Style.fg scheme.comment) ++
      Text.plain "\n" ++ Text.styled "/check" (Style.fg scheme.cyan) ++
        Text.styled "               check with default" (Style.dim <+> Style.fg scheme.comment) ++
      Text.plain "\n" ++ Text.styled "/run --prover eprover" (Style.fg scheme.cyan) ++
        Text.styled "  run one prover" (Style.dim <+> Style.fg scheme.comment) ++
      Text.plain "\n\n" ++ Text.styled "ENTER" (Style.bold <+> Style.fg scheme.foreground) ++
        Text.styled " submit  •  " (Style.dim <+> Style.fg scheme.comment) ++
        Text.styled "CTRL-N" (Style.bold <+> Style.fg scheme.foreground) ++
        Text.styled " newline" (Style.dim <+> Style.fg scheme.comment) ++
      Text.plain "\n" ++ Text.styled "/help" (Style.fg scheme.cyan) ++
        Text.styled "             command reference" (Style.dim <+> Style.fg scheme.comment)
  let content := if outer < 82 then
      align inner .center (truncate inner (compactLeft ++ Text.plain "\n\n" ++ compactRight))
    else
      let paneSpace := inner - 1
      let leftWidth := paneSpace * 56 / 100
      let rightWidth := paneSpace - leftWidth
      let left := align leftWidth .center (truncate leftWidth leftContent)
      let right := align rightWidth .left (truncate rightWidth rightContent)
      columns [leftWidth, rightWidth] 1 [left, right] []
        (Text.styled "│" (Style.fg scheme.selection))
  box content
    { title := some (Text.styled s!" oatp repl v{version} "
        (Style.bold <+> Style.fg scheme.orange))
      , titleAlignment := .left, borderStyle := Style.fg scheme.orange
      , maxWidth := some outer }

private def compactHeader (scheme : ColorScheme) (width : Nat) : Text :=
  let outer := frameWidth width
  let title := s!" oatp repl v{version} "
  let used := title.length + 2
  Text.styled "──" (Style.fg scheme.selection) ++
    Text.styled title (Style.bold <+> Style.fg scheme.orange) ++
    Text.styled (String.ofList (List.replicate (if outer > used then outer - used else 0) '─'))
      (Style.fg scheme.selection)

def reportAtScreenRow (app : App) (size : Size) (row : Nat) : Option Nat :=
  let width := frameWidth size.columns
  let head := if app.entries.isEmpty then banner app.theme width else compactHeader app.theme width
  let bodyStart := head.height + 1
  if row < bodyStart then none else reportAtTranscriptRow app width (row - bodyStart)

def prompt (scheme : ColorScheme) (width : Nat) (state : Repl.State)
    (focused : Bool := true) : Text :=
  let outer := frameWidth width
  let input := box (Text.styled "› " (Style.bold <+> Style.fg scheme.orange) ++
      TermColor.Repl.renderMultilineTextInputBody
        { width := max 1 (boxInnerWidth outer - 2), textStyle := Style.fg scheme.foreground
          cursorStyle := Style.reverse } state.input focused)
      { chars := { topLeft := '╭', topRight := '╮', bottomLeft := '╰', bottomRight := '╯' }
        , borderStyle := Style.fg scheme.selection, maxWidth := some outer }
  match state.completion with
  | none => input
  | some menu => input ++ Text.plain "\n" ++ TermColor.Repl.renderCompletionMenu
      { width := max 1 (boxInnerWidth outer - 2)
        selectedStyle := Style.bg scheme.selection <+> Style.fg scheme.foreground
        textStyle := Style.fg scheme.foreground
        kindStyle := Style.dim <+> Style.fg scheme.comment } menu

private def footer (app : App) (width : Nat) : Text :=
  let outer := frameWidth width
  let state := if app.busy then "[BUSY]" else "[READY]"
  let closeRun := appKeyLabel .closeRun .runInput
  let closeProvers := appKeyLabel .closeProvers .proversInput
  let closeState := appKeyLabel .closeState .stateInput
  let closeHistory := appKeyLabel .closeHistory .historyInput
  let focus := appKeyLabel .focusDrawer .default
  let next := appKeyLabel .runNext .run
  let previous := appKeyLabel .runPrevious .run
  let inspect := appKeyLabel .runInspect .run
  let pageUp := appKeyLabel .transcriptPageUp .default
  let pageDown := appKeyLabel .transcriptPageDown .default
  let hint := if app.panelFocus == .drawer && app.runOpen then
      if outer < stateDrawerMinWidth then s!"{closeRun} main • {next}/{previous}"
      else s!"{closeRun} main • {next}/{previous} prover • {inspect} details"
    else if app.panelFocus == .drawer && app.stateOpen then
      if outer < stateDrawerMinWidth then s!"{closeState} main • {next}/{previous}"
      else s!"{closeState} main • {next}/{previous} focus • Enter toggle"
    else if app.panelFocus == .drawer && app.proversOpen then
      if outer < stateDrawerMinWidth then s!"{closeProvers} main • {next}/{previous}"
      else s!"{closeProvers} main • {next}/{previous} prover • Space toggle"
    else if app.panelFocus == .drawer && app.historyOpen then s!"{closeHistory} main"
    else if app.busy then s!"{appKeyLabel .openRun .default} run • input"
    else if app.stateOpen || app.proversOpen || app.historyOpen || app.runOpen then
      if outer < stateDrawerMinWidth then s!"input • {focus}"
      else s!"input active • {focus} focus drawer"
    else s!"/help • {pageUp}/{pageDown} scroll • {appKeyLabel .openRun .default} runs"
  let leftWidth := outer * 2 / 3
  let rightWidth := outer - leftWidth
  let notice := app.statusNotice.map (fun value => s!"  • {value}") |>.getD ""
  let prover := app.defaultProver.map ProverReference.display |>.getD "auto"
  let metadata := if outer < stateDrawerMinWidth then s!"  theory={app.theory}{notice}"
    else s!"  theory={app.theory} • prover={prover}{notice}"
  let left := Text.styled state
      (Style.bold <+> Style.fg (if app.busy then app.theme.yellow else app.theme.green)) ++
    Text.styled metadata
      (Style.dim <+> Style.fg app.theme.comment)
  columns [leftWidth, rightWidth] 0
    [ truncate leftWidth left
    , truncate rightWidth
        (Text.styled hint (Style.dim <+> Style.fg app.theme.comment)) ] [.left, .left]

def selectedText (app : App) (size : Size) : String :=
  let width := frameWidth size.columns
  let head := if app.entries.isEmpty then banner app.theme width else compactHeader app.theme width
  let foot := prompt app.theme width app.repl (app.panelFocus == .main) ++
    Text.plain "\n" ++ footer app width
  let used := head.height + foot.height + 2
  let budget := if size.rows > used then size.rows - used else 1
  let bodyStart := head.height + 1
  let lines := selectionLines app width budget
  match app.selectionStart, app.selectionEnd with
  | some (_, start), some (_, finish) =>
      let low := min start finish
      let high := max start finish
      let low := if low > bodyStart then low - bodyStart else 0
      let high := if high > bodyStart then high - bodyStart else 0
      String.intercalate "\n" (lines.drop low |>.take (high - low + 1))
  | _, _ => ""

private def calcContent (app : App) (size : Size) : Text :=
  let width := frameWidth size.columns
  let head := if app.entries.isEmpty then banner app.theme width else compactHeader app.theme width
  let foot := prompt app.theme width app.repl (app.panelFocus == .main) ++
    Text.plain "\n" ++ footer app width
  let used := head.height + foot.height + 2
  let budget := if size.rows > used then size.rows - used else 1
  let body := transcript app width budget (head.height + 1)
  head ++ Text.plain "\n" ++ fillHeight budget body ++ Text.plain "\n" ++ foot

def screen (app : App) (size : Size) : Text :=
  let width := frameWidth size.columns
  let size := { size with columns := width }
  let drawerOpen := app.runOpen || app.proversOpen ||
    (app.panelFocus == .drawer && (app.stateOpen || app.historyOpen))
  let content := match drawerOpen, stateDrawerWidths width with
    | true, some (leftWidth, rightWidth) =>
        let left := calcContent app { size with columns := leftWidth }
        let left := if app.panelFocus == .main then left else dimText left
        let right := if app.runOpen then runPanel app rightWidth size.rows
          else if app.historyOpen then historyPanel app rightWidth size.rows
          else if app.proversOpen then proverPanel app rightWidth size.rows
          else contextPanel app rightWidth size.rows
        let right := if app.panelFocus == .drawer then right else dimText right
        columns [leftWidth, rightWidth] 2 [left, right] []
          (Text.styled "│" (Style.fg app.theme.selection))
    | _, _ => calcContent app size
  opaqueScreen app.theme size content app.copyPending

end OATP.ReplView
