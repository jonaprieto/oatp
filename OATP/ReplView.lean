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

def theme : ColorScheme := ColorScheme.catppuccin

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
  stateOpen : Bool := true
  running : Bool := true
  status : String := "ready"
  busy : Bool := false
  jobResult : Option JobResult := none
  goal : Option OATP.GoalSnapshot := none
  translation : Option String := none
  term : Option OATP.Lean.Repl.RenderedTerm := none
  leanRuntime : Option OATP.Lean.Repl.Runtime := none
  leanGoal : Option OATP.Lean.Repl.Goal := none

def fallbackSize : Size := { columns := 100, rows := 28 }

def inputConfig : TextInputConfig := { width := 160, maxLength := 16_384 }

def multilineConfig : MultilineConfig :=
  { text := inputConfig, lineBreak := .ctrl 'n' }

private def frameWidth (width : Nat) : Nat := max 48 width

private def joinLines : List Text → Text
  | [] => Text.empty
  | line :: lines => lines.foldl (fun result next => result ++ Text.plain "\n" ++ next) line

private def fitText (width : Nat) (value : String) : Text :=
  Text.plain (String.ofList (value.toList.take width))

private def transcriptLine (entry : TranscriptEntry) : Text :=
  let input := Text.styled s!"[{entry.cell}] › {entry.input}" (Style.fg theme.cyan)
  let marker := if entry.ok then "=" else "!"
  let style := if entry.ok then Style.fg theme.green else Style.fg theme.red
  let output := match entry.output.splitOn "\n" with
    | [] => Text.empty
    | line :: rest =>
        let first := Text.styled s!"    {marker} " (Style.bold <+> style) ++ Text.plain line
        rest.foldl (fun output line => output ++ Text.plain "\n      " ++ Text.plain line)
          first
  input ++ Text.plain "\n" ++ output

private def transcript (entries : List TranscriptEntry) : Text :=
  let visible := entries.reverse.take 14
  if visible.isEmpty then
    Text.styled "Enter a TPTP statement or /help." (Style.dim <+> Style.fg theme.comment)
  else
    joinLines (visible.map (fun entry => transcriptLine entry))

private def formulaLine (formula : FormulaView) : Text :=
  Text.plain s!"{formula.cell} {formula.role} {formula.name}: {formula.formula}"

private def symbolLine (symbol : Symbol) : Text :=
  let kind := match symbol.kind with
    | .variable => "var"
    | .constant => "const"
    | .function => "fun"
    | .predicate => "pred"
  Text.plain s!"{kind} {symbol.name}/{symbol.arity}"

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
    [ Text.styled "CONJECTURES / FORMULAS" (Style.bold <+> Style.fg theme.purple) ] ++
    (if formulas.isEmpty then [Text.plain "(none)"] else formulas.map formulaLine) ++
    [ Text.styled "SYMBOLS" (Style.bold <+> Style.fg theme.purple) ] ++
    (if symbols.isEmpty then [Text.plain "(none)"] else symbols.map symbolLine) ++
    [ Text.styled "PROBLEM" (Style.bold <+> Style.fg theme.purple)
    , fitText (max 1 (width - 4)) problem
    , Text.styled "LEAN GOAL" (Style.bold <+> Style.fg theme.purple)
    , Text.plain goal
    , Text.styled "TRANSLATED TPTP" (Style.bold <+> Style.fg theme.purple)
    , fitText (max 1 (width - 4)) translation
    , Text.styled "TERM" (Style.bold <+> Style.fg theme.purple)
    , Text.plain term ]
  box body { title := some (Text.styled "state" (Style.bold <+> Style.fg theme.orange))
           , borderStyle := Style.fg theme.selection
           , maxWidth := some width }

private def banner (width : Nat) : Text :=
  box (joinLines [
    Text.styled "OATP REPL" (Style.bold <+> Style.fg theme.orange),
    Text.plain "TPTP context • prover artifacts • kernel-checked Lean terms",
    Text.styled "Type fof(...), or /help" (Style.dim <+> Style.fg theme.comment)
  ]) { title := some (Text.styled s!" oatp-repl v{version} "
                    (Style.bold <+> Style.fg theme.orange))
      , borderStyle := Style.fg theme.orange
      , maxWidth := some width }

def prompt (width : Nat) (state : Repl.State) : Text :=
  box (Text.styled "› " (Style.fg theme.orange) ++
    TermColor.Repl.renderMultilineTextInputBody
      { width := max 1 (width - 4), textStyle := Style.fg theme.foreground
        cursorStyle := Style.reverse } state.input true)
    { borderStyle := Style.fg theme.selection, maxWidth := some width }

def screen (app : App) (size : Size) : Text :=
  let width := frameWidth size.columns
  let header := if app.entries.isEmpty then banner width else
    Text.styled s!" oatp-repl v{version} " (Style.bold <+> Style.fg theme.orange) ++
      Text.styled (String.ofList (List.replicate (max 0 (width - version.length - 12)) '─'))
        (Style.fg theme.selection)
  let state := if app.busy then "busy" else app.status
  let footer := Text.styled s!"[{state}]  /help  /state  /history  /quit"
    (Style.dim <+> Style.fg theme.comment)
  let prompt := prompt width app.repl
  let body := joinLines [header, transcript app.entries, prompt, footer]
  if app.stateOpen && width >= 100 then
    columns [width * 3 / 5, width - width * 3 / 5] 1
      [body, contextPanel app (width - width * 3 / 5)] []
      (Text.styled "│" (Style.fg theme.selection))
  else body

end OATP.ReplView
