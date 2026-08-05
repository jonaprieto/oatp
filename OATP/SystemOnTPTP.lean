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
open OATP.Http.Form

structure Config where
  endpoint : String := "https://tptp.org/cgi-bin/SystemOnTPTP"
  systemLabel : String
  timeLimit : Nat := 30
  maxBodyBytes : Nat := 4 * 1024 * 1024
  deriving BEq, DecidableEq, Repr

inductive ResponseError where
  | httpStatus (statusCode : Nat)
  | missingStatus
  | unsupportedStatus (value : String)
  deriving Repr

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
  body := encodeUrlEncoded (fields config problem)
  headers := #["Content-Type: application/x-www-form-urlencoded"]
  maxSeconds := config.timeLimit + 10
  maxBodyBytes := config.maxBodyBytes

def submit (config : Config) (problem : Problem) :
  IO (Except Http.Error Http.Response) :=
  Http.requestWithTransport (request config problem)

def parseResponse (config : Config) (problem : Problem) (response : Http.Response) :
    Except ResponseError Artifact := do
  if response.statusCode < 200 || response.statusCode ≥ 300 then
    throw (.httpStatus response.statusCode)
  let token ← match SZSStatus.tokenFromOutput response.body with
    | some token => Except.ok token
    | none => Except.error .missingStatus
  let status ← match SZSStatus.ofString token with
    | some status => Except.ok status
    | none => Except.error (.unsupportedStatus token)
  pure {
    prover := { name := config.systemLabel }
    status
    problemName := some problem.name
    stdout := response.body
    stderr := response.stderr
  }

end OATP.SystemOnTPTP
