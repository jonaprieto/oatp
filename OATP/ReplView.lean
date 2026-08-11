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
  transcriptScroll : Nat := 0
  repl : Repl.State := {}
  stateOpen : Bool := false
  historyOpen : Bool := false
  proversOpen : Bool := false
  proverFocus : Nat := 0
  proverChoices : Array String := #[]
  selectionStart : Option (Nat × Nat) := none
  selectionEnd : Option (Nat × Nat) := none
  copyPending : Option String := none
  contextFocus : Nat := 0
  contextExpanded : Array Bool := #[false, false, false, false, false, false]
  running : Bool := true
  status : String := "ready"
  statusNotice : Option String := none
  theme : ColorScheme := aurora
  themeName : String := defaultThemeName
  theory : String := "fof"
  defaultProver : String := ""
  enabledProvers : Array String := #[]
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

private def diagnosticView (scheme : ColorScheme) (width : Nat) (sources : Sources) (diagnostic : Diagnostic) : Text :=
  TermColor.Diagnostics.render sources diagnostic
    { width := max 1 (frameWidth width - 2), contextLines := 0, hyperlinks := false } scheme

private def identifierChar (character : Char) : Bool :=
  character.isAlpha || character.isDigit || character == '_' || character == '$' || character == '\''

private def symbolStyle (scheme : ColorScheme) (symbols : Array Symbol) (token : String) : Style :=
  let lower := token.toLower
  if token.startsWith "/" then Style.bold <+> Style.fg scheme.orange
  else if token.startsWith "--" then Style.fg scheme.blue
  else if token == "-" then Style.fg scheme.blue
  else if token.startsWith "$" then Style.fg scheme.blue
  else if lower == "cnf" || lower == "fof" || lower == "tff" || lower == "thf" then
    Style.bold <+> Style.fg scheme.cyan
  else if lower == "axiom" || lower == "conjecture" || lower == "type" ||
      lower == "definition" || lower == "theorem" || lower == "hypothesis" ||
      lower == "assumption" || lower == "lemma" || lower == "corollary" ||
      lower == "negated_conjecture" || lower == "plain" || lower == "interpretation" ||
      lower == "logic" || lower == "fi_domain" || lower == "fi_functors" ||
      lower == "fi_predicates" then Style.fg scheme.purple
  else if lower == "error" || lower == "failed" || lower == "unknown" then
    Style.bold <+> Style.fg scheme.red
  else match symbols.find? (fun symbol => symbol.name == token) with
  | some symbol => match symbol.kind with
      | .variable => Style.fg scheme.yellow
      | .constant => Style.fg scheme.green
      | .function => Style.fg scheme.green
      | .predicate => Style.fg scheme.cyan
  | none =>
      match token.toList.head? with
      | some character =>
          if character.isUpper || character.isDigit then Style.fg scheme.yellow
          else Style.fg scheme.foreground
      | none => Style.fg scheme.foreground

private def operatorStyle (scheme : ColorScheme) (token : String) : Style :=
  if token == "!" || token == "?" then Style.fg scheme.pink
  else if token == "(" || token == ")" || token == "[" || token == "]" || token == "," ||
      token == ":" || token == "." || token == ";" then Style.fg scheme.purple
  else Style.fg scheme.blue

private def semanticText (scheme : ColorScheme) (symbols : Array Symbol) (value : String) : Text :=
  let flush := fun (state : Text × String) =>
    if state.2.isEmpty then state
    else (state.1 ++ Text.styled state.2 (symbolStyle scheme symbols state.2), "")
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
        (state.1 ++ Text.styled token (operatorStyle scheme token), "")
      else
        (state.1 ++ Text.plain token, "")
  (flush (value.toList.foldl step (Text.empty, ""))).1

private def semanticFormula (scheme : ColorScheme) (formula : FormulaView) : Text :=
  semanticText scheme formula.symbols formula.formula

private def formulaLine (scheme : ColorScheme) (formula : FormulaView) : Text :=
  Text.styled s!"{formula.cell} {formula.role} " (Style.dim <+> Style.fg scheme.comment) ++
    Text.styled formula.name (Style.bold <+> Style.fg scheme.cyan) ++
    Text.styled ": " (Style.dim <+> Style.fg scheme.comment) ++ semanticFormula scheme formula

private def transcriptLine (scheme : ColorScheme) (symbols : Array Symbol) (width : Nat) (entry : TranscriptEntry) : Text :=
  let input := Text.styled s!"[{entry.cell}] " (Style.dim <+> Style.fg scheme.comment) ++
    Text.styled "› " (Style.bold <+> Style.fg scheme.orange) ++
    semanticText scheme symbols entry.input
  let marker := if entry.ok then "=" else "!"
  let style := if entry.ok then Style.fg scheme.green else Style.fg scheme.red
  let marker := Text.styled s!"  {marker} " (Style.bold <+> style)
  let output := match entry.diagnostic with
    | some diagnostic =>
        match splitLines (diagnosticView scheme width entry.sources diagnostic) with
        | [] => marker
        | line :: rest =>
            let first := marker ++ line
            rest.foldl (fun output line => output ++ Text.plain "\n    " ++ line) first
    | none =>
        match entry.output.splitOn "\n" with
        | [] => marker
        | line :: rest =>
            let first := marker ++ semanticText scheme symbols line
            rest.foldl (fun output line => output ++ Text.plain "\n    " ++ semanticText scheme symbols line)
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

def contextSectionCount : Nat := 6

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
  let problem := if app.session.problemSource.isEmpty then
      "(no problem)"
    else
      String.intercalate "\n" (app.session.problemSource.splitOn "\n" |>.take 3)
  let goal := match app.goal with
    | none => Text.plain "(no Lean goal)"
    | some goal => joinLines <| goal.context.toList.map (semanticText app.theme app.session.symbols) ++
        [semanticText app.theme app.session.symbols s!"⊢ {goal.target}"]
  let term := match app.term with
    | none => Text.plain "(no checked term)"
    | some term => semanticText app.theme app.session.symbols s!"{term.term} : {term.type}"
  let translation := match app.translation with
    | none => "(no Lean → TPTP translation)"
    | some source => String.intercalate "\n" (source.splitOn "\n" |>.take 3)
  [ (Text.plain s!"FORMULAS ({app.session.formulas.size})"
    , joinLines (if formulas.isEmpty then [] else formulas.map (formulaLine app.theme)))
  , (Text.plain s!"SYMBOLS ({app.session.symbols.size})"
    , joinLines (symbols.map (symbolLine app.theme)))
  , (Text.plain "PROBLEM", semanticText app.theme app.session.symbols problem)
  , (Text.plain "LEAN GOAL", goal)
  , (Text.plain "LEAN → TPTP", semanticText app.theme app.session.symbols translation)
  , (Text.plain "CHECKED TERM", term) ]

private def contextExpandedAt (app : App) (index : Nat) : Bool :=
  app.contextExpanded.getD index false

private def contextState (app : App) (index : Nat) : CollapsibleState :=
  { expanded := contextExpandedAt app index
    focused := app.contextFocus == index }

private def contextRenders (app : App) (width : Nat) : List CollapsibleRender :=
  let innerWidth := max 1 (boxInnerWidth width)
  let rec go : List (Text × Text) → Nat → List CollapsibleRender
    | [], _ => []
    | (summary, body) :: sections, index =>
        renderCollapsible (contextWidgetConfig app.theme) innerWidth summary body (contextState app index) ::
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

private def updateContextExpanded (app : App) (index : Nat) (expanded : Bool) : App :=
  { app with contextExpanded := app.contextExpanded.set! index expanded }

def focusContext (app : App) (index : Nat) : App :=
  { app with contextFocus := min (contextSectionCount - 1) index }

def focusNextContext (app : App) : App :=
  focusContext app ((app.contextFocus + 1) % contextSectionCount)

def focusPreviousContext (app : App) : App :=
  focusContext app (if app.contextFocus == 0 then contextSectionCount - 1 else app.contextFocus - 1)

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

def toggleFocusedProver (app : App) : App :=
  if app.proverChoices.isEmpty then app else
    let name := app.proverChoices[app.proverFocus]!
    let enabled := app.enabledProvers.any (· == name)
    let updated := { app with enabledProvers := if enabled then
        app.enabledProvers.filter (· != name)
      else app.enabledProvers.push name }
    let updated := { updated with proverSelectionSet := true }
    { updated with statusNotice := some s!"{name} {if enabled then "disabled" else "enabled"}" }

def contextTargetNames : List String :=
  ["formulas", "symbols", "problem", "goal", "translation", "term"]

def contextTargetOfString (value : String) : Option Nat :=
  match value.toLower with
  | "form" | "formula" | "formulas" => some 0
  | "symbol" | "symbols" => some 1
  | "problem" => some 2
  | "goal" | "lean" => some 3
  | "translation" | "tptp" => some 4
  | "term" | "checked-term" | "checked" => some 5
  | _ => none

def openContextTarget (app : App) (target : String) : Option App :=
  if target.toLower == "all" then
    let app := { app with stateOpen := true }
    some { app with contextExpanded := #[true, true, true, true, true, true] }
  else
    match contextTargetOfString target with
    | some index => some <| expandFocusedContext <| focusContext { app with stateOpen := true } index
    | none => none

private def contextPanel (app : App) (width height : Nat) : Text :=
  let body := joinLines (contextTexts app width)
  let innerWidth := boxInnerWidth width
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) body)
  box body { title := some (Text.styled "context" (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg app.theme.selection, maxWidth := some width }

private def historyPanel (app : App) (width height : Nat) : Text :=
  let rows := app.session.history.toList.reverse.take 18
  let body := if rows.isEmpty then
      Text.styled "No commands yet." (Style.dim <+> Style.fg app.theme.comment)
    else
      joinLines (rows.map fun entry =>
        Text.styled s!"[{entry.cell}] " (Style.dim <+> Style.fg app.theme.comment) ++
          semanticText app.theme app.session.symbols entry.input ++
          Text.plain "\n" ++
          Text.styled "  = " (Style.bold <+> Style.fg app.theme.green) ++
          semanticText app.theme app.session.symbols
            (fitText (max 1 (width - 6)) entry.result).plainText)
  let innerWidth := boxInnerWidth width
  let body := padRight innerWidth (fillHeight (max 1 (height - 2)) body)
  box body { title := some (Text.styled "history • active" (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg app.theme.selection, maxWidth := some width }

private def proverPanel (app : App) (width height : Nat) : Text :=
  let rows := if app.proverChoices.isEmpty then
      [Text.styled "No installed provers." (Style.dim <+> Style.fg app.theme.comment)]
    else app.proverChoices.toList.mapIdx fun index name =>
      let checked := app.enabledProvers.any (· == name)
      let marker := if checked then "[x]" else "[ ]"
      let style := if index == app.proverFocus then Style.reverse else {}
      Text.styled s!"{marker} {name}" style
  let body := padRight (boxInnerWidth width) (fillHeight (max 1 (height - 2)) (joinLines rows))
  box body { title := some (Text.styled "provers • active"
      (Style.bold <+> Style.fg app.theme.cyan))
           , borderStyle := Style.fg app.theme.selection, maxWidth := some width }

private def mascot (scheme : ColorScheme) : Text :=
  joinLines [ Text.styled "  ◆  " (Style.fg scheme.yellow)
            , Text.styled " /|\\ " (Style.fg scheme.cyan)
            , Text.styled "◆─┼─◆" (Style.fg scheme.green)
            , Text.styled " \\|/ " (Style.fg scheme.blue) ]

private def banner (scheme : ColorScheme) (width : Nat) : Text :=
  let outer := frameWidth width
  let inner := boxInnerWidth outer
  let paneSpace := inner - 1
  let leftWidth := max 24 (paneSpace * 56 / 100)
  let rightWidth := max 24 (paneSpace - leftWidth)
  let left := align leftWidth .center (truncate leftWidth <|
    Text.styled "OATP REPL" (Style.bold <+> Style.fg scheme.foreground) ++
      Text.plain "\n\n" ++ mascot scheme ++ Text.plain "\n\n" ++
      Text.styled "TPTP • Lean • ATP" (Style.fg scheme.comment))
  let right := align rightWidth .left (truncate rightWidth <|
    Text.styled "START HERE" (Style.bold <+> Style.fg scheme.orange) ++
      Text.plain "\n" ++ Text.styled "/to-lean p => p" (Style.fg scheme.cyan) ++
      Text.plain "\n/state  context drawer" ++
      Text.plain "\n/run --prover eprover" ++
      Text.plain "\n\nenter submit • ctrl-n newline" ++
      Text.plain "\n/help commands")
  box (columns [leftWidth, rightWidth] 1 [left, right] []
    (Text.styled "│" (Style.fg scheme.selection)))
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

def prompt (scheme : ColorScheme) (width : Nat) (state : Repl.State) : Text :=
  let outer := frameWidth width
  let input := box (Text.styled "› " (Style.bold <+> Style.fg scheme.orange) ++
      TermColor.Repl.renderMultilineTextInputBody
        { width := max 1 (boxInnerWidth outer - 2), textStyle := Style.fg scheme.foreground
          cursorStyle := Style.reverse } state.input true)
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
  let hint := if app.stateOpen then "H main • J/K focus • Enter open"
    else if app.proversOpen then "H main • J/K prover • Space toggle"
    else if app.historyOpen then "H main • /history close"
    else "/help • PgUp/PgDn scroll • /state"
  let leftWidth := outer * 2 / 3
  let rightWidth := outer - leftWidth
  let notice := app.statusNotice.map (fun value => s!"  • {value}") |>.getD ""
  let prover := if app.defaultProver.isEmpty then "auto" else app.defaultProver
  let left := Text.styled state (Style.bold <+> Style.fg (if app.busy then app.theme.yellow else app.theme.green)) ++
    Text.styled s!"  theory={app.theory} • prover={prover}{notice}"
      (Style.dim <+> Style.fg app.theme.comment)
  columns [leftWidth, rightWidth] 0
    [ truncate leftWidth left
    , truncate rightWidth (Text.styled hint (Style.dim <+> Style.fg app.theme.comment)) ] [.left, .left]

def selectedText (app : App) (size : Size) : String :=
  let width := frameWidth size.columns
  let head := if app.entries.isEmpty then banner app.theme width else compactHeader app.theme width
  let foot := prompt app.theme width app.repl ++ Text.plain "\n" ++ footer app width
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
  let foot := prompt app.theme width app.repl ++ Text.plain "\n" ++ footer app width
  let used := head.height + foot.height + 2
  let budget := if size.rows > used then size.rows - used else 1
  let body := transcript app width budget (head.height + 1)
  head ++ Text.plain "\n" ++ fillHeight budget body ++ Text.plain "\n" ++ foot

def screen (app : App) (size : Size) : Text :=
  let width := frameWidth size.columns
  let size := { size with columns := width }
  let drawerOpen := app.stateOpen || app.historyOpen || app.proversOpen
  let content := match drawerOpen, stateDrawerWidths width with
    | true, some (leftWidth, rightWidth) =>
        let left := calcContent app { size with columns := leftWidth }
        let right := if app.historyOpen then historyPanel app rightWidth size.rows
          else if app.proversOpen then proverPanel app rightWidth size.rows
          else contextPanel app rightWidth size.rows
        columns [leftWidth, rightWidth] 2 [left, right] []
          (Text.styled "│" (Style.fg app.theme.selection))
    | _, _ => calcContent app size
  opaqueScreen app.theme size content app.copyPending

end OATP.ReplView
