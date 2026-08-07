/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core
import TPTP

/-!
# OATP.TPTP: OATP's format-layer adapter

TPTP/TSTP syntax belongs to the standalone `lean-grip-tptp` package. This module
keeps OATP's import path and adds only the small constructors needed by the
ATP domain model.
-/

namespace OATP.TPTP

abbrev Name := _root_.TPTP.Name
abbrev Kind := _root_.TPTP.Kind
abbrev Role := _root_.TPTP.Role
abbrev Statement := _root_.TPTP.Statement
abbrev Include := _root_.TPTP.Include
abbrev Item := _root_.TPTP.Item
abbrev Document := _root_.TPTP.Document

def parse (source : String) : Except Grip.ParseError Document :=
  _root_.TPTP.parseString source

def parseStatement (source : String) : Except Grip.ParseError Statement :=
  _root_.TPTP.parseStatementString source

namespace Syntax

abbrev Term := _root_.TPTP.Formula.Term
abbrev Formula := _root_.TPTP.Formula.Expr

def parseFormula (source : String) : Except Grip.ParseError Formula :=
  _root_.TPTP.Formula.parseFormulaString source

end Syntax

namespace Statement

def ofFof (name : String) (role : Role) (formula : _root_.TPTP.Formula.Expr) :
    Except String _root_.TPTP.Statement := do
  let formula ← formula.toTPTP
  pure {
    kind := .fof
    name := .bare name
    role
    formula
  }

def parseFormula (statement : _root_.TPTP.Statement) :
    Except Grip.ParseError _root_.TPTP.Formula.Expr :=
  _root_.TPTP.Statement.parseFormula statement

end Statement

end OATP.TPTP

namespace OATP

def Problem.ofStatement (name : String) (statement : _root_.TPTP.Statement) : Problem where
  name := name
  source := _root_.TPTP.Statement.render statement

end OATP
