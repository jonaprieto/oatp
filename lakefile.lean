import Lake
open Lake DSL

package «oatp» where
  version := v!"0.7.11"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require grip from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "v0.3.6"

require «grip-json» from git
  "https://github.com/jonaprieto/lean-grip-json.git"
  @ "v0.1.7"

require tptp from git
  "https://github.com/jonaprieto/lean-grip-tptp.git"
  @ "v0.5.8"

require «termcolor» from git
  "https://github.com/jonaprieto/lean-termcolor.git"
  @ "v1.1.7"

require «termcolor-diagnostics» from git
  "https://github.com/jonaprieto/lean-termcolor-diagnostics.git"
  @ "v0.1.18"

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "v0.3.8"

require «termcolor-widgets» from git
  "https://github.com/jonaprieto/lean-termcolor-widgets.git"
  @ "v0.1.15"

require «termcolor-repl» from git
  "https://github.com/jonaprieto/lean-termcolor-repl.git"
  @ "v0.8.8"

require argus from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "v0.5.5"

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

lean_lib «OATP.ReplApp» where
  roots := #[`Repl]
  srcDir := "examples"

@[test_driver]
lean_exe «tests» where
  root := `Tests
  srcDir := "test"
