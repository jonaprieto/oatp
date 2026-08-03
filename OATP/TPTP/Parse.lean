/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.TPTP.Syntax

/-!
# OATP.TPTP.Parse: supported first-order formula parser

This parser intentionally accepts the same unquoted first-order fragment that
`OATP.TPTP.Syntax` renders. It is not a complete TPTP or TSTP parser.
-/

namespace OATP.TPTP.Syntax

private structure Cursor where
  input : List Char
  offset : Nat

private abbrev Parser (α : Type) := StateT Cursor (Except String) α

private def isSpace (character : Char) : Bool :=
  character == ' ' || character == '\n' || character == '\r' || character == '\t'

private def peek : Parser (Option Char) := do
  pure (← get).input.head?

private def consume : Parser (Option Char) := do
  let cursor ← get
  match cursor.input with
  | [] => pure none
  | character :: rest =>
      set ({ input := rest, offset := cursor.offset + 1 } : Cursor)
      pure (some character)

private partial def skipSpaces : Parser Unit := do
  match ← peek with
  | some character =>
      if isSpace character then
        let _ ← consume
        skipSpaces
      else
        pure ()
  | none => pure ()

private def consumeText (text : String) : Parser Bool := do
  skipSpaces
  let cursor ← get
  let characters := text.toList
  if cursor.input.take characters.length == characters then
    set ({ input := cursor.input.drop characters.length, offset :=
      cursor.offset + characters.length } : Cursor)
    pure true
  else
    pure false

private def expectText (text : String) : Parser Unit := do
  unless ← consumeText text do
    throw s!"expected `{text}`"

private partial def identifierTail (accumulator : List Char) : Parser String := do
  match ← peek with
  | some character =>
      if character.isAlphanum || character == '_' || character == '$' then
        let _ ← consume
        identifierTail (character :: accumulator)
      else
        pure (String.ofList accumulator.reverse)
  | none => pure (String.ofList accumulator.reverse)

private def identifier : Parser String := do
  skipSpaces
  match ← consume with
  | some character =>
      unless character.isAlpha || character == '_' || character == '$' do
        throw "expected an identifier"
      identifierTail [character]
  | none => throw "expected an identifier"

mutual

private partial def parseTerm : Parser Term := do
  let name ← identifier
  let arguments ← if ← consumeText "(" then
      if ← consumeText ")" then
        pure #[]
      else
        parseTerms #[]
    else
      pure #[]
  let first := name.toList.head!
  if first.isUpper || first == '_' then
    if arguments.isEmpty then pure (.var name)
    else throw s!"variable `{name}` cannot have arguments"
  else if arguments.isEmpty then
    pure (.constant name)
  else
    pure (.function name arguments)

private partial def parseTerms (accumulator : Array Term) : Parser (Array Term) := do
  let term ← parseTerm
  if ← consumeText "," then
    parseTerms (accumulator.push term)
  else
    let _ ← expectText ")"
    pure (accumulator.push term)

end

mutual

private partial def parseFormulaP : Parser Formula := parseIff

private partial def parseAtom : Parser Formula := do
  let predicate ← identifier
  let arguments ← if ← consumeText "(" then
      if ← consumeText ")" then
        pure #[]
      else
        parseTerms #[]
    else
      pure #[]
  pure (.atom predicate arguments)

private partial def parseUnary : Parser Formula := do
  if ← consumeText "~" then
    pure (.not (← parseUnary))
  else if ← consumeText "!" then
    expectText "["
    let varName ← identifier
    expectText "]"
    expectText ":"
    pure (.forall varName (← parseFormulaP))
  else if ← consumeText "?" then
    expectText "["
    let varName ← identifier
    expectText "]"
    expectText ":"
    pure (.exists varName (← parseFormulaP))
  else if ← consumeText "(" then
    let formula ← parseFormulaP
    expectText ")"
    pure formula
  else
    let atom ← parseAtom
    if atom == .atom "$true" #[] then pure .truth
    else if atom == .atom "$false" #[] then pure .falsity
    else pure atom

private partial def parseAndRest (left : Formula) : Parser Formula := do
  if ← consumeText "&" then
    parseAndRest (.and left (← parseUnary))
  else
    pure left

private partial def parseAnd : Parser Formula := do
  parseUnary >>= parseAndRest

private partial def parseOrRest (left : Formula) : Parser Formula := do
  if ← consumeText "|" then
    parseOrRest (.or left (← parseAnd))
  else
    pure left

private partial def parseOr : Parser Formula := do
  parseAnd >>= parseOrRest

private partial def parseImplies : Parser Formula := do
  let left ← parseOr
  if ← consumeText "=>" then
    pure (.implies left (← parseImplies))
  else
    pure left

private partial def parseIff : Parser Formula := do
  let left ← parseImplies
  if ← consumeText "<=>" then
    pure (.iff left (← parseIff))
  else
    pure left

end

def parseFormula (source : String) : Except String Formula := do
  let (formula, cursor) ← (do
    let formula ← parseFormulaP
    skipSpaces
    pure formula : Parser Formula).run {
      input := source.toList
      offset := 0
    }
  if cursor.input.isEmpty then
    pure formula
  else
    .error s!"unexpected input at character {cursor.offset}"

end OATP.TPTP.Syntax
