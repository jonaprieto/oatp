/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core
import OATP.Http

/-!
# OATP.SystemOnTPTP: typed form adapter

The adapter preserves the old service's field names at one boundary. The rest
of OATP deals in typed requests and raw responses, so a change in the service
form does not leak into the core model.
-/

namespace OATP.SystemOnTPTP

open OATP

structure Field where
  name : String
  value : String
  deriving BEq, DecidableEq, Repr

private def hexDigits : Array Char :=
  #[
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  ]

private def isUnreserved (byte : UInt8) : Bool :=
  (byte >= 48 && byte <= 57) ||
  (byte >= 65 && byte <= 90) ||
  (byte >= 97 && byte <= 122) ||
  byte == 45 || byte == 46 || byte == 95 || byte == 126

private def encodeByte (byte : UInt8) : String :=
  if isUnreserved byte then
    Char.ofNat byte.toNat |>.toString
  else
    let n := byte.toNat
    s!"%{hexDigits[n / 16]!}{hexDigits[n % 16]!}"

def encodeComponent (value : String) : String :=
  String.join (value.toUTF8.toList.map encodeByte)

def encodeForm (fields : Array Field) : String :=
  String.intercalate "&" <| fields.toList.map fun field =>
    s!"{encodeComponent field.name}={encodeComponent field.value}"

structure Config where
  endpoint : String := "https://tptp.org/cgi-bin/SystemOnTPTP"
  systemLabel : String
  timeLimit : Nat := 30
  deriving BEq, DecidableEq, Repr

def fields (config : Config) (problem : Problem) : Array Field :=
  let label := config.systemLabel
  #[
    { name := "NoHTML", value := "-P" },
    { name := "SubmitButton", value := "RunSelectedSystems" },
    { name := "TPTPProblem", value := problem.source },
    { name := s!"System___{label}", value := label },
    { name := s!"Command___{label}", value := "default" },
    { name := s!"Format___{label}", value := "tptp:raw" },
    { name := s!"TimeLimit___{label}", value := toString config.timeLimit },
    { name := s!"Transform___{label}", value := "none" }
  ]

def request (config : Config) (problem : Problem) : Http.Request where
  method := .post
  url := config.endpoint
  body := encodeForm (fields config problem)
  headers := #["Content-Type: application/x-www-form-urlencoded"]
  maxSeconds := config.timeLimit + 10

def submit (config : Config) (problem : Problem) :
    IO (Except Http.Error Http.Response) :=
  Http.requestWithCurl (request config problem)

end OATP.SystemOnTPTP
