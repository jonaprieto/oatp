/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Lean
import OATP.TPTP

/-!
# OATP.Translate: conservative Lean goal translation

Only proposition-shaped constants and local propositions, together with the
non-dependent propositional connectives, cross this boundary.  Terms,
dependent hypotheses, equality, and arbitrary applications stay unsupported;
silently inventing first-order terms here would make an external result look
more precise than the Lean goal it came from.
-/

namespace OATP.Lean

open _root_.Lean
open _root_.Lean.Meta

structure GoalTranslation where
  problem : OATP.Problem
  assumptions : Array String := #[]
  target : String
  deriving Repr

private def hexDigit (value : Nat) : Char :=
  if value < 10 then Char.ofNat (48 + value) else Char.ofNat (97 + (value - 10))

private def symbolName (raw : String) : String :=
  let body := raw.toUTF8.toList.map fun byte =>
    let value := byte.toNat
    s!"{hexDigit (value / 16)}{hexDigit (value % 16)}"
  "oatp_" ++ if body.isEmpty then "atom" else String.intercalate "" body

private def unsupported (expression : Expr) : MetaM (Except String TPTP.Formula.Expr) := do
  let rendered ← ppExpr expression
  pure <| Except.error (s!"unsupported Lean proposition `{rendered}`; supported fragment is " ++
    "propositional logic over named atoms")

private partial def translateProp (expression : Expr) :
    MetaM (Except String TPTP.Formula.Expr) := do
  let expression ← instantiateMVars expression
  let (function, arguments) := expression.getAppFnArgs
  if function == ``True && arguments.isEmpty then
    return .ok .truth
  if function == ``False && arguments.isEmpty then
    return .ok .falsity
  if function == ``Not && arguments.size == 1 then
    match ← translateProp arguments[0]! with
    | .ok body => return .ok (.not body)
    | .error message => return .error message
  if function == ``And && arguments.size == 2 then
    match ← translateProp arguments[0]!, ← translateProp arguments[1]! with
    | .ok left, .ok right => return .ok (.and left right)
    | .error message, _ | _, .error message => return .error message
  if function == ``Or && arguments.size == 2 then
    match ← translateProp arguments[0]!, ← translateProp arguments[1]! with
    | .ok left, .ok right => return .ok (.or left right)
    | .error message, _ | _, .error message => return .error message
  if function == ``Iff && arguments.size == 2 then
    match ← translateProp arguments[0]!, ← translateProp arguments[1]! with
    | .ok left, .ok right => return .ok (.iff left right)
    | .error message, _ | _, .error message => return .error message
  match expression with
  | .forallE _ premise body _ =>
      if !(← isProp premise) || body.hasLooseBVars then
        return ← unsupported expression
      match ← translateProp premise, ← translateProp (body.instantiate1 (mkConst ``True)) with
      | .ok left, .ok right => return .ok (.implies left right)
      | .error message, _ | _, .error message => return .error message
  | .const name _ =>
      if ← isProp expression then
        return .ok (.atom (symbolName s!"{name}") #[])
      return ← unsupported expression
  | .fvar fvarId =>
      if ← isProp expression then
        return .ok (.atom (symbolName s!"{fvarId.name}") #[])
      return ← unsupported expression
  | _ => return ← unsupported expression

private def renderStatement (name : String) (role : _root_.TPTP.Role)
    (formula : TPTP.Formula.Expr) : Except String String := do
  let statement ← OATP.TPTP.Statement.ofFof name role formula
  pure <| _root_.TPTP.Statement.render statement

private def renderFormula (formula : TPTP.Formula.Expr) : Except String String :=
  formula.toTPTP

def translateGoal (mvarId : MVarId) : MetaM (Except String GoalTranslation) :=
  mvarId.withContext do
    let target ← instantiateMVars (← mvarId.getType)
    unless ← isProp target do
      let rendered ← ppExpr target
      return .error s!"goal `{rendered}` is not a proposition"
    let targetFormula ← translateProp target
    let targetFormula ← match targetFormula with
      | .ok formula => pure formula
      | .error message => return .error message
    let targetText ← match renderFormula targetFormula with
      | .ok text => pure text
      | .error message => return .error message
    let mut sources : Array String := #[]
    let mut assumptions : Array String := #[]
    let mut index := 0
    let localContext ← getLCtx
    for fvarId in localContext.getFVarIds do
      let declaration := localContext.get! fvarId
      unless declaration.isImplementationDetail do
        if ← isProp declaration.type then
          let formula ← match ← translateProp (← instantiateMVars declaration.type) with
            | .ok formula => pure formula
            | .error message => return .error message
          let source ← match renderStatement s!"hypothesis_{index}" .axiom formula with
            | .ok source => pure source
            | .error message => return .error message
          sources := sources.push source
          assumptions := assumptions.push source
          index := index + 1
    let targetSource ← match renderStatement "goal" .conjecture targetFormula with
      | .ok source => pure source
      | .error message => return .error message
    sources := sources.push targetSource
    pure <| .ok {
      problem := { name := "lean-goal", source := String.intercalate "\n" sources.toList }
      assumptions
      target := targetText
    }

end OATP.Lean
