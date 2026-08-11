/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import Argus

/-!
# OATP.Argus: shared option grammar

The batch CLI and REPL intentionally have different positional arguments, but their prover
catalogue and resource flags must mean the same thing. These nested option records keep that
grammar in one inspectable Argus spec.
-/

namespace OATP.Argus

open _root_.Argus

def endpointSpec := Spec.opt (Spec.flag "endpoint" none
  "SystemOnTPTP endpoint" Param.str)

argus_opts ResourceOptions where
  timeout : Option Nat := Spec.opt (Spec.flag "timeout" (some 't')
    "Wall-clock limit" Param.duration);
  maxOutput : Option Nat := Spec.opt (Spec.flag "max-output" none
    "Maximum captured output" Param.bytes)

argus_opts CatalogueOptions where
  endpoint : Option String := endpointSpec;
  refresh : Bool := Spec.switch "refresh" none "Refresh the online prover catalogue";
  noCache : Bool := Spec.switch "no-cache" none "Do not read or write the catalogue cache"

argus_opts RemoteOptions where
  endpoint : Option String := endpointSpec;
  resources : ResourceOptions := ResourceOptions.spec

end OATP.Argus
