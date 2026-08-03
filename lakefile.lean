import Lake
open Lake DSL

package «oatp» where
  version := v!"0.1.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require grip from git
  "https://github.com/jonaprieto/lean-grip.git"
  @ "986b668d3da7919f24e2612770c227a2e8bb1fb5"

require «termcolor» from git
  "https://github.com/jonaprieto/lean-termcolor.git"
  @ "708bfb2a314ed81aeb9bf5461895a55fe3aa49e8"

require «termcolor-layout» from git
  "https://github.com/jonaprieto/lean-termcolor-layout.git"
  @ "3b3e49b0fbbd02c8d6b02de33ba1d1ed356ce77d"

require «termcolor-diagnostics» from git
  "https://github.com/jonaprieto/lean-termcolor-diagnostics.git"
  @ "722c7e48707577b05bc5beffe8c969cd497e221c"

require «termcolor-widgets» from git
  "https://github.com/jonaprieto/lean-termcolor-widgets.git"
  @ "114f2ae0a2711d6f93fa2de09a884a56b4241f1a"

require «termcolor-terminal» from git
  "https://github.com/jonaprieto/lean-termcolor-terminal.git"
  @ "c812eb51bf49b645cecc6335a1f6631dd2a13bb0"

require «argus» from git
  "https://github.com/jonaprieto/lean-argus.git"
  @ "15154adf9deaaa46c466f2b2acdbbc948e0a8c4c"

@[default_target]
lean_lib «OATP» where
  globs := #[.andSubmodules `OATP]

lean_lib «OATP.Properties» where
  roots := #[`OATP.Properties]
  globs := #[.andSubmodules `OATP.Properties]

lean_exe «demo» where
  root := `Demo
  srcDir := "examples"

lean_exe «tests» where
  root := `Tests
  srcDir := "test"
