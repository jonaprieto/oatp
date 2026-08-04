import Lake
open Lake DSL

package «oatp» where
  version := v!"0.2.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require grip from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "17bed154d8188650bf8dd458ec44385ce72d6ba4"

require tptp from git
  "https://github.com/jonaprieto/lean-tptp.git"
  @ "853ebb7334186c4c3bbc26d181ff1cc0e6937014"

require «termcolor» from git
  "https://github.com/jonaprieto/lean-termcolor.git"
  @ "0d5a6ba9ac64912a91fd724eb986a18fc0793b98"

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

lean_exe «tests» where
  root := `Tests
  srcDir := "test"
