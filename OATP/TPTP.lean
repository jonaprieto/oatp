/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import Grip
import OATP.Core

/-!
# OATP.TPTP: first positioned TPTP statement parser

The prototype intentionally parses the stable outer TPTP statement envelope
and preserves the formula body verbatim. Grip supplies the byte parser and
positioned failures; semantic formula parsing will be added after the envelope
and artifact workflow are stable.
-/

namespace OATP.TPTP

open Grip GParser

inductive Role where
  | axiom
  | conjecture
  | negatedConjecture
  | hypothesis
  | unknown
  deriving BEq, DecidableEq, Repr

def Role.toString : Role → String
  | .axiom => "axiom"
  | .conjecture => "conjecture"
  | .negatedConjecture => "negated_conjecture"
  | .hypothesis => "hypothesis"
  | .unknown => "unknown"

instance : ToString Role where
  toString := Role.toString

def Role.ofString : String → Role
  | "axiom" => .axiom
  | "conjecture" => .conjecture
  | "negated_conjecture" => .negatedConjecture
  | "hypothesis" => .hypothesis
  | _ => .unknown

structure Statement where
  kind : String
  name : String
  role : Role
  formula : String
  deriving BEq, DecidableEq, Repr

def identifier : Parser String :=
  GParser.capture (GParser.weakenFallible (GParser.takeWhile1 (fun b =>
    Ascii.isAlphaNum b || b == 95 || b == 36)
  ))

def spaces : Parser Unit :=
  GParser.map (fun _ => ()) (GParser.weakenFallible GParser.ws)

def statementParser : Parser Statement := do
  let kind ← GParser.capture (GParser.weakenFallible
    (GParser.string "fof" <|> GParser.string "cnf"))
  spaces
  GParser.ch '('
  spaces
  let name ← identifier
  spaces
  GParser.ch ','
  spaces
  let roleName ← identifier
  spaces
  GParser.ch ','
  let formula ← GParser.capture (GParser.weakenFallible
    (GParser.takeWhile1 (fun b => b != 41 && b != 46)))
  spaces
  GParser.ch ')'
  GParser.ch '.'
  pure { kind, name, role := Role.ofString roleName, formula := formula.trimAscii.toString }

def parseStatement (source : String) : Except String Statement :=
  match statementParser.parse source.toUTF8 with
  | .ok statement => .ok statement
  | .error error => .error (error.pretty source.toUTF8)

def Problem.ofStatement (name : String) (statement : Statement) : Problem where
  name := name
  source := s!"{statement.kind}({statement.name}, {statement.role}, {statement.formula})."

end OATP.TPTP
