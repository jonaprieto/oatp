/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import Grip
import OATP.Core

/-!
# OATP.TPTP: positioned TPTP/TSTP statement envelopes

The prototype parses the stable outer `fof`/`cnf` envelope, including balanced
terms, quoted atoms, and optional TSTP annotations. Formula semantics remain
opaque text until the supported Lean fragment is defined.
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
  annotations : Option String := none
  deriving BEq, DecidableEq, Repr

def identifier : Parser String :=
  GParser.capture (GParser.weakenFallible (GParser.takeWhile1 (fun b =>
    Ascii.isAlphaNum b || b == 95 || b == 36)
  ))

def spaces : Parser Unit :=
  GParser.map (fun _ => ()) (GParser.weakenFallible GParser.ws)

private def escapedChunk : GParser conditional String :=
  gdo
    let _ ← GParser.byte 92
    let escaped ← GParser.satisfy (fun _ => true)
    return String.ofList ['\\', Char.ofNat escaped.toNat]

private def quotedChunk (quote : UInt8) : GParser conditional String :=
  gdo
    let _ ← GParser.byte quote
    let chunks ← GParser.many (GParser.alt
      (GParser.capture (GParser.takeWhile1 (fun b => b != quote && b != 92)))
      escapedChunk)
    let _ ← GParser.byte quote
    return String.ofList [Char.ofNat quote.toNat] ++ String.join chunks ++
      String.ofList [Char.ofNat quote.toNat]

private def parenthesizedPiece (body : GParser conditional String) :
    GParser conditional String :=
  gdo
    let _ ← GParser.ch '('
    let value ← body
    let _ ← GParser.ch ')'
    return "(" ++ value ++ ")"

private def formulaPiece (body : GParser conditional String) : GParser conditional String :=
  GParser.dispatch (fun b =>
    if b == 34 then quotedChunk 34
    else if b == 39 then quotedChunk 39
    else if b == 40 then parenthesizedPiece body
    else GParser.capture (GParser.takeWhile1 (fun byte =>
      byte != 40 && byte != 41 && byte != 34 && byte != 39)))

def formulaBody : GParser conditional String :=
  GParser.fix (fun body => GParser.map String.join (GParser.many1 (formulaPiece body)))

private def splitAnnotations (body : String) : String × Option String :=
  let rec go : List Char → Nat → Option Char → Bool → List Char → String × Option String
    | [], _, _, _, acc => (String.ofList acc.reverse, none)
    | character :: rest, depth, quote, escaped, acc =>
        match quote with
        | some delimiter =>
            if escaped then go rest depth quote false (character :: acc)
            else if character == '\\' then go rest depth quote true (character :: acc)
            else if character == delimiter then go rest depth none false (character :: acc)
            else go rest depth quote false (character :: acc)
        | none =>
            if character == '"' || character == '\'' then
              go rest depth (some character) false (character :: acc)
            else if character == '(' then
              go rest (depth + 1) none false (character :: acc)
            else if character == ')' && depth > 0 then
              go rest (depth - 1) none false (character :: acc)
            else if character == ',' && depth == 0 then
              (String.ofList acc.reverse, some (String.ofList rest))
            else go rest depth none false (character :: acc)
  go body.toList 0 none false []

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
  spaces
  let body ← GParser.weakenFallible formulaBody
  spaces
  GParser.ch ')'
  GParser.ch '.'
  let _ ← GParser.weakenFallible GParser.eof
  let (formula, annotations) := splitAnnotations body
  pure {
    kind,
    name,
    role := Role.ofString roleName,
    formula := formula.trimAscii.toString,
    annotations := annotations.map (·.trimAscii.toString)
  }

def parseStatement (source : String) : Except String Statement :=
  match statementParser.parse source.toUTF8 with
  | .ok statement => .ok statement
  | .error error => .error (error.pretty source.toUTF8)

def Problem.ofStatement (name : String) (statement : Statement) : Problem where
  name := name
  source :=
    let annotationText := Option.map (fun value => s!", {value}") statement.annotations |>.getD ""
    s!"{statement.kind}({statement.name}, {statement.role}, {statement.formula})" ++
      annotationText ++ "."

end OATP.TPTP
