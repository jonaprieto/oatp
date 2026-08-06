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
  @ "0d5a6ba9ac64912a91fd724eb986a18fc0793b98"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "7dc4a0bc6336e22129be2527f40edf1cf96cbc65"

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
