import Lake
open Lake DSL

package «oatp» where
  version := v!"0.2.3"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require grip from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "v0.1.0"

require tptp from git
  "https://github.com/jonaprieto/lean-tptp.git"
  @ "v0.5.1"

require «termcolor» from git
  "https://github.com/jonaprieto/lean-termcolor.git"
  @ "ac9a102562fa65435365758cf5fe5ac95c6a7a92"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "6e903b606f6cb64476e7e95364050e3a0935c717"

@[default_target]
lean_lib «OATP» where
  roots := #[`OATP]

lean_lib «OATP.Properties» where
  roots := #[`OATP.Properties]
  globs := #[.andSubmodules `OATP.Properties]

lean_exe «demo» where
  root := `Demo
  srcDir := "examples"

lean_exe «proof-demo» where
  root := `ProofDemo
  srcDir := "examples"

lean_exe «oatp» where
  root := `Cli
  srcDir := "examples"

lean_exe «tests» where
  root := `Tests
  srcDir := "test"
