/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jonathan Prieto-Cubides
-/

import OATP.Core

/-!
# OATP.Events: renderer-independent search events

Terminal widgets and future editor widgets consume these events. Keeping them
pure prevents presentation code from becoming part of the proof or transport
layer.
-/

namespace OATP

structure GoalSnapshot where
  title : String
  context : Array String := #[]
  target : String
  deriving BEq, DecidableEq, Repr

structure TacticAttempt where
  tactic : String
  outcome : String
  elapsedMs : Nat := 0
  deriving BEq, DecidableEq, Repr

inductive SearchEvent where
  | goal (snapshot : GoalSnapshot)
  | attempt (attempt : TacticAttempt)
  | result (outcome : Outcome)
  | note (message : String)
  deriving Repr

end OATP
