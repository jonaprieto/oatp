/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

/-!
# OATP.TPTP.Syntax: supported first-order formula fragment

This small AST is for generating TPTP `fof` formulas. Rendering validates the
supported unquoted identifier subset and rejects unbound variables.
-/

namespace OATP.TPTP.Syntax

inductive Term where
  | var (name : String)
  | constant (name : String)
  | function (name : String) (arguments : Array Term)
  deriving BEq, Repr

inductive Formula where
  | atom (predicate : String) (arguments : Array Term)
  | truth
  | falsity
  | not (body : Formula)
  | and (left right : Formula)
  | or (left right : Formula)
  | implies (left right : Formula)
  | iff (left right : Formula)
  | forall (varName : String) (body : Formula)
  | exists (varName : String) (body : Formula)
  deriving BEq, Repr

private def join (values : List String) : String :=
  String.intercalate ", " values

private def validName (first : Char → Bool) (name : String) : Bool :=
  match name.toList with
  | [] => false
  | character :: rest => first character && rest.all (fun value =>
      Char.isAlphanum value || value == '_' || value == '$')

private def symbolName (kind name : String) : Except String String :=
  if validName (fun character => character.isLower || character == '$') name then
    .ok name
  else
    .error s!"invalid TPTP {kind} `{name}`"

private def variableName (name : String) : Except String String :=
  if validName (fun character => character.isUpper || character == '_') name then
    .ok name
  else
    .error s!"invalid TPTP variable `{name}`"

partial def Term.toTPTP (term : Term) (bound : Array String := #[]) :
    Except String String :=
  match term with
  | .var name =>
      if bound.toList.contains name then
        .ok name
      else
        .error s!"unbound TPTP variable `{name}`"
  | .constant name => symbolName "symbol" name
  | .function name arguments => do
      let name ← symbolName "function" name
      let arguments ← arguments.toList.mapM (fun term => term.toTPTP bound)
      if arguments.isEmpty then
        pure name
      else
        pure s!"{name}({join arguments})"

partial def Formula.toTPTP (formula : Formula) (bound : Array String := #[]) :
    Except String String :=
  match formula with
  | .atom predicate arguments => do
      let predicate ← symbolName "predicate" predicate
      let arguments ← arguments.toList.mapM (fun term => term.toTPTP bound)
      if arguments.isEmpty then
        pure predicate
      else
        pure s!"{predicate}({join arguments})"
  | .truth => pure "$true"
  | .falsity => pure "$false"
  | .not body => do
      let body ← body.toTPTP bound
      pure s!"~({body})"
  | .and left right => do
      let left ← left.toTPTP bound
      let right ← right.toTPTP bound
      pure s!"({left} & {right})"
  | .or left right => do
      let left ← left.toTPTP bound
      let right ← right.toTPTP bound
      pure s!"({left} | {right})"
  | .implies left right => do
      let left ← left.toTPTP bound
      let right ← right.toTPTP bound
      pure s!"({left} => {right})"
  | .iff left right => do
      let left ← left.toTPTP bound
      let right ← right.toTPTP bound
      pure s!"({left} <=> {right})"
  | .forall varName body => do
      let varName ← variableName varName
      let body ← body.toTPTP (bound.push varName)
      pure s!"![{varName}] : ({body})"
  | .exists varName body => do
      let varName ← variableName varName
      let body ← body.toTPTP (bound.push varName)
      pure s!"?[{varName}] : ({body})"

end OATP.TPTP.Syntax
