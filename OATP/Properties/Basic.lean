/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import OATP.Core
import OATP.Events
import OATP.TPTP
import OATP.Term

namespace OATP.Properties

open OATP OATP.TPTP

theorem empty_plain_render : OATP.Term.renderPlain #[] = "" := by
  rfl

theorem note_plain_render :
    OATP.Term.renderPlain #[.note "ready"] = "note: ready" := by
  rfl

end OATP.Properties
