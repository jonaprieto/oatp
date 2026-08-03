import Lake
open Lake DSL

package «oatp» where
  version := v!"0.2.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require grip from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "986b668d3da7919f24e2612770c227a2e8bb1fb5"

require tptp from git
  "https://github.com/jonaprieto/lean-tptp.git"
  @ "dafe15ae3cd57a4dad4947d303b8086693d19e30"

require «termcolor» from git
  "https://github.com/jonaprieto/lean-termcolor.git"
  @ "708bfb2a314ed81aeb9bf5461895a55fe3aa49e8"

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
