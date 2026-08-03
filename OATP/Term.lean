/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import OATP.Core
import OATP.Events
import TermColor

/-!
# OATP.Term: first TermColor-backed output view
-/

namespace OATP.Term

open OATP TermColor

def eventText : SearchEvent → Text
  | .goal snapshot =>
      Text.styled "goal: " Style.bold ++ Text.plain snapshot.title ++
        Text.plain "\n" ++ Text.plain "⊢ " ++ Text.plain snapshot.target
  | .attempt attempt =>
      Text.styled "attempt: " Style.cyan ++ Text.plain attempt.tactic ++
        Text.plain s!" ({attempt.outcome}, {attempt.elapsedMs}ms)"
  | .result outcome => Text.styled "result: " Style.bold ++ Text.plain outcome.status
  | .note message => Text.styled "note: " Style.dim ++ Text.plain message

def renderPlain (events : Array SearchEvent) : String :=
  String.intercalate "\n" (events.toList.map (fun event => (eventText event).plainText))

def renderAnsi16 (events : Array SearchEvent) : String :=
  String.intercalate "\n"
    (events.toList.map (fun event => Text.render RenderTarget.ansi16 (eventText event)))

end OATP.Term
