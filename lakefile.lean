import Lake
open Lake DSL

package «oatp» where
  version := v!"0.2.2"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require grip from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "17bed154d8188650bf8dd458ec44385ce72d6ba4"

require tptp from git
  "https://github.com/jonaprieto/lean-tptp.git"
  @ "v0.5.0"

require «termcolor» from git
  "https://github.com/jonaprieto/lean-termcolor.git"
  @ "ac9a102562fa65435365758cf5fe5ac95c6a7a92"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "c38b78d113d56fccc300d77c9707aee7a55dc719"

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
