/-
Copyright (c) 2026 Jonathan Cubides. All rights reserved.
Released under Apache-2.0 license as described in the file LICENSE.
Authors: Jonathan Cubides
-/

import OATP.SystemOnTPTP

/-!
# OATP.ProverReference: typed local and online prover selections

The reference kind is shared by batch and interactive frontends. Keeping the service prefix
conversion here prevents either frontend from depending on the other.
-/

namespace OATP

inductive ProverReferenceKind where
  | local
  | online
  deriving BEq, DecidableEq, Repr, Inhabited

structure ProverReference where
  name : String
  kind : ProverReferenceKind
  deriving BEq, DecidableEq, Repr, Inhabited

namespace ProverReference

def fromLocal (name : String) : ProverReference := { name, kind := .local }

def fromOnline (name : String) : ProverReference :=
  { name := OATP.SystemOnTPTP.onlineSystemId name, kind := .online }

def ofString (name : String) : ProverReference :=
  if OATP.SystemOnTPTP.isOnlineReference name then fromOnline name else fromLocal name

def fromPersisted (value : String) : Option ProverReference :=
  if value.startsWith "local:" then
    let name := value.drop "local:".length |>.toString
    if name.isEmpty then none else some (fromLocal name)
  else if value.startsWith "online:" then
    let name := value.drop "online:".length |>.toString
    if name.isEmpty then none else some (fromOnline name)
  else if value.isEmpty then none else some (ofString value)

def persisted (reference : ProverReference) : String :=
  match reference.kind with
  | .local => "local:" ++ reference.name
  | .online => "online:" ++ reference.name

def display (reference : ProverReference) : String :=
  match reference.kind with
  | .local => reference.name
  | .online => OATP.SystemOnTPTP.onlineReference reference.name

end ProverReference
end OATP
