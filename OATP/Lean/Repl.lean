/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import Lean
import OATP.Lean
import OATP.Proof
import OATP.TPTP
import OATP.Translate

/-!
# OATP.Lean.Repl: stateful kernel-facing REPL bridge

The bridge keeps Lean's core and metavariable state between commands. TPTP formulas are translated
to a fresh propositional Lean context only when they stay inside the supported propositional
fragment; all returned proof terms still pass through `OATP.Lean.checkAndAssign`.
-/

namespace OATP.Lean.Repl

open _root_.Lean
open _root_.Lean.Meta
open OATP

structure Runtime where
  coreContext : Core.Context
  coreState : Core.State
  metaState : Meta.State

structure Goal where
  mvarId : MVarId
  source : String
  atoms : Array String := #[]

structure RenderedTerm where
  term : String
  type : String
  checked : Bool
  deriving Repr

private def coreContext : Core.Context := {
  fileName := "<oatp-repl>"
  fileMap := FileMap.ofString ""
  options := {}
  quotContext := `OATP
}

def create : IO Runtime := do
  Lean.initSearchPath (← Lean.getBuildDir)
  let env ← Lean.importModules #[{ module := `Init.Prelude }] {}
  pure {
    coreContext := coreContext
    coreState := { env }
    metaState := {}
  }

private def runMeta {α : Type} (runtime : Runtime) (action : MetaM α) :
    IO (α × Runtime) := do
  let (value, coreState, metaState) ← action.toIO runtime.coreContext runtime.coreState {}
    runtime.metaState
  pure (value, { runtime with coreState, metaState })

private def addAtom (atoms : Array String) (name : String) : Array String :=
  if atoms.contains name then atoms else atoms.push name

private abbrev TranslationM := ExceptT String MetaM

private def translationError {α : Type} (message : String) : TranslationM α :=
  ExceptT.mk (pure (.error message))

private partial def atomNames (formula : _root_.TPTP.Formula.Expr) (atoms : Array String) :
    Array String :=
  match formula with
  | .atom predicate arguments =>
      if arguments.isEmpty then addAtom atoms predicate else atoms
  | .truth | .falsity => atoms
  | .not body => atomNames body atoms
  | .and left right | .or left right | .implies left right | .iff left right =>
      atomNames right (atomNames left atoms)
  | .forall _ body | .exists _ body => atomNames body atoms

private def withAtoms {α : Type} (atoms : List String)
    (action : Array (String × Expr) → TranslationM α) : MetaM (Except String α) :=
  match atoms with
  | [] => (action #[]).run
  | atom :: rest =>
    withLocalDeclD (Name.mkSimple atom) (mkSort .zero) fun fvar =>
      withAtoms rest fun locals => action (locals.push (atom, fvar))

private def lookupAtom (atoms : Array (String × Expr)) (name : String) : Option Expr :=
  atoms.find? (·.1 == name) |>.map Prod.snd

private partial def toLean (atoms : Array (String × Expr))
    (formula : _root_.TPTP.Formula.Expr) : TranslationM Expr := do
  match formula with
  | .atom predicate arguments =>
      if !arguments.isEmpty then
        translationError
          s!"predicate `{predicate}` has terms; Lean translation supports propositions"
      else
        match lookupAtom atoms predicate with
        | some atom => pure atom
        | none => translationError s!"missing Lean atom `{predicate}`"
  | .truth => pure (mkConst ``True)
  | .falsity => pure (mkConst ``False)
  | .not body =>
      let body ← toLean atoms body
      ExceptT.lift <| mkAppM ``Not #[body]
  | .and left right => binary ``And left right
  | .or left right => binary ``Or left right
  | .implies left right => do
      let left ← toLean atoms left
      let right ← toLean atoms right
      ExceptT.lift <| mkArrow left right
  | .iff left right => binary ``Iff left right
  | .forall _ _ | .exists _ _ =>
      translationError "quantified TPTP formulas need a declared Lean signature"
where
  binary (name : Name) (left right : _root_.TPTP.Formula.Expr) : TranslationM Expr := do
    let left ← toLean atoms left
    let right ← toLean atoms right
    ExceptT.lift <| mkAppM name #[left, right]

private def makeGoal (source : String) (formula : _root_.TPTP.Formula.Expr) :
    MetaM (Except String Goal) := do
  let atoms := atomNames formula #[]
  withAtoms atoms.toList fun locals => do
    let target ← toLean locals formula
    let goal ← ExceptT.lift <| mkFreshExprMVar (some target)
    pure { mvarId := goal.mvarId!, source, atoms }

def goalFromFormula (runtime : Runtime) (source : String) :
    IO (Except String (Runtime × Goal)) := do
  match OATP.TPTP.Syntax.parseFormula source with
  | .error error => pure (.error (error.pretty source.toUTF8))
  | .ok formula =>
      let (result, runtime) ← runMeta runtime (makeGoal source formula)
      pure <| result.map fun goal => (runtime, goal)

def snapshot (runtime : Runtime) (goal : Goal) : IO (Runtime × OATP.GoalSnapshot) := do
  let (value, runtime) ← runMeta runtime (OATP.Lean.snapshot goal.mvarId)
  pure (runtime, value)

def translateToTPTP (runtime : Runtime) (goal : Goal) :
    IO (Runtime × Except String OATP.Lean.GoalTranslation) := do
  let (value, runtime) ← runMeta runtime (OATP.Lean.translateGoal goal.mvarId)
  pure (runtime, value)

def reconstruct (runtime : Runtime) (goal : Goal) (step : OATP.Proof.Step) :
    IO (Runtime × Except String RenderedTerm) := do
  let action : MetaM (Except String RenderedTerm) := do
    match ← OATP.Proof.reconstruct goal.mvarId step with
    | .error message => pure (.error message)
    | .ok checked =>
        let term ← ppExpr checked.proof
        let type ← ppExpr checked.target
        pure (.ok { term := s!"{term}", type := s!"{type}", checked := true })
  let (value, runtime) ← runMeta runtime action
  pure (runtime, value)

end OATP.Lean.Repl
