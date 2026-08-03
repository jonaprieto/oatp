/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

/-!
# OATP.TPTP.Syntax: supported first-order formula fragment

This small AST is for generating TPTP `fof` formulas. Identifiers are kept as
strings because the surrounding TPTP envelope already uses source-level names;
callers are responsible for supplying valid TPTP identifiers.
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

partial def Term.toTPTP : Term → String
  | .var name => name
  | .constant name => name
  | .function name arguments =>
      if arguments.isEmpty then name
      else s!"{name}({join (arguments.toList.map Term.toTPTP)})"

partial def Formula.toTPTP : Formula → String
  | .atom predicate arguments =>
      if arguments.isEmpty then predicate
      else s!"{predicate}({join (arguments.toList.map Term.toTPTP)})"
  | .truth => "$true"
  | .falsity => "$false"
  | .not body => s!"~({body.toTPTP})"
  | .and left right => s!"({left.toTPTP} & {right.toTPTP})"
  | .or left right => s!"({left.toTPTP} | {right.toTPTP})"
  | .implies left right => s!"({left.toTPTP} => {right.toTPTP})"
  | .iff left right => s!"({left.toTPTP} <=> {right.toTPTP})"
  | .forall varName body => s!"![{varName}] : ({body.toTPTP})"
  | .exists varName body => s!"?[{varName}] : ({body.toTPTP})"

instance : ToString Term where
  toString := Term.toTPTP

instance : ToString Formula where
  toString := Formula.toTPTP

end OATP.TPTP.Syntax
