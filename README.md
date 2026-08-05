# oatp

Lean 4 ATP orchestration with proof-artifact-first results, TPTP support, and
rich terminal output.

[![CI](https://github.com/jonaprieto/oatp/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/oatp/actions/workflows/ci.yml)
[![Lean 4](https://img.shields.io/badge/Lean-v4.32.2-blue)](lean-toolchain)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

> Prototype: the public API is intentionally small. External ATP results remain
> untrusted until a kernel-checked reconstruction path accepts them.

## Background

OATP grows out of [online-atps](https://github.com/jonaprieto/online-atps), an
earlier Haskell project I created for working with online automated theorem
provers and reconstructing their proofs for Agda. I also contributed to
[ASR/Apia](https://github.com/asr/apia). Now, with Lean 4 and the surrounding
ecosystem, we can do better: local and remote prover backends, reusable TPTP
parsing, reproducible artifacts, readable diagnostics, and an explicit
boundary between an external candidate and a kernel-checked Lean proof. OATP
is that more complete redesign.

## What exists

- pure prover, SZS status, limits, artifact, outcome, and search-event models;
- the standalone [lean-tptp](https://github.com/jonaprieto/lean-tptp) package,
  which owns total Grip-backed TPTP/TSTP parsing and the first-order formula
  layer;
- bounded HTTPS requests through argv-safe `curl` (preferred) or `wget`
  fallback transport, with a post-capture response-size check;
- a pure SystemOnTPTP response normalizer for HTTP status and SZS results,
  retaining problem names in artifacts;
- a local argv-safe prover runner with stdin delivery, timeout termination
  requests, and output limits;
- reproducible, no-network E, Vampire, and Metis containers for local
  development;
- a Lean metavariable snapshotter and kernel-facing candidate checker;
- a small kernel-checked propositional reconstruction calculus;
- TermColor plain and ANSI-16 event rendering;
- an `argus` CLI with structured usage diagnostics for local and online runs;
- TTY-only indeterminate progress for local and online waits;
- separate properties and executable tests.

The central trust rule is explicit: an ATP `Theorem` result is a `candidate`,
not a Lean proof. The current reconstruction path only covers the small
kernel-checked propositional calculus; it does not turn arbitrary ATP output
into a verified result.

## Quick start

```sh
lake build
lake exe demo
lake exe proof-demo
lake exe oatp --help
lake exe oatp doctor
lake exe tests
python3 scripts/style-check.py
python3 scripts/check-axioms.py
```

The demo shows a goal, tactic attempts, an external candidate, and the
proof-artifact trust boundary in both plain and ANSI output. The proof demo
constructs and assigns a kernel-checked `True` proof. The CLI runs a local
prover or submits a problem to SystemOnTPTP:

```sh
lake exe oatp local --executable cat test/fixtures/system-on-tptp/theorem.txt
lake exe oatp online --system vampire test/fixtures/system-on-tptp/theorem.txt
```

`oatp doctor` reports the platform, HTTP transport availability, Docker, and
common local ATP executables. `curl` is preferred; `wget` is used only when
`curl` is unavailable.

When stdout is a capable TTY, local and online waits use the pure
`termcolor-widgets` indeterminate bar through `termcolor-terminal`; redirected
output stays static and machine-readable. The bar's frame and label are
caller-owned, so OATP does not add a timer or terminal-control dependency to
its pure event model.

## Install

```lean
require oatp from git
  "https://github.com/jonaprieto/oatp.git"
  @ "main"
```

The prototype currently targets Lean 4.32.2, TPTP 0.5.0, and Argus 0.2.4.
Pin releases or commits for reproducible builds.

## Reproducible local provers

Docker is optional. The repository includes small, no-network images for the
three direct-TPTP provers currently covered by the prototype:

```sh
docker build -f docker/eprover/Dockerfile \
  -t oatp/eprover:bookworm-2.6 .
docker build -f docker/vampire/Dockerfile \
  -t oatp/vampire:bookworm-5.0.1 .
docker build -f docker/metis/Dockerfile \
  -t oatp/metis:bookworm-2.4.20260305 .
```

Run a TPTP problem through the hardened generic wrapper:

```sh
scripts/run-tptp-docker.sh oatp/eprover:bookworm-2.6 < problem.p
scripts/run-tptp-docker.sh oatp/vampire:bookworm-5.0.1 --time_limit 5 < problem.p
scripts/run-tptp-docker.sh oatp/metis:bookworm-2.4.20260305 --time-limit 5 < problem.p
```

The E-specific wrapper remains available for existing callers:

```lean
let command : OATP.Process.Command := {
  executable := "./scripts/run-eprover-docker.sh"
  arguments := #["--cpu-limit=5"]
}
```

For host-based smoke testing, install the direct-TPTP tools with Homebrew:

```sh
brew install eprover vampire polyml
```

Metis is not the Homebrew `metis` formula: that formula is a graph-partitioning
library. Build Metis from its [official release](https://github.com/gilith/metis/releases)
with Poly/ML, then put `bin/polyml/metis` on your `PATH`.

Prover9 is also useful for future testing (`brew install prover9`), but the
available Homebrew release uses LADR input rather than this TPTP fixture. Z3
and cvc5 target SMT-LIB; they need a separate translation boundary.

The wrapper disables networking, drops Linux capabilities, uses a read-only
root filesystem, and applies CPU, memory, PID, and temporary-space limits. A
Lean caller can use the same wrapper through `OATP.Process.Command`; Docker is
therefore an environment boundary, not a library dependency. The returned
TSTP/SZS output remains an untrusted candidate until reconstruction accepts it.

## Ecosystem

OATP keeps pure data separate from IO. TPTP syntax is owned by the standalone
`lean-tptp` package, which uses `grip` for byte-oriented parsing; OATP adds
the prover, HTTP, process, and reconstruction boundaries around it. `termcolor`
provides pure terminal text. `argus` supplies typed flags, derived help,
completions, and source-annotated usage errors; `termcolor-terminal` owns
TTY detection, redraw, and live progress; `termcolor-widgets` owns the pure
indeterminate bar. OATP only supplies the backend operation and label.
ProofWidgets4 support is available as the optional
[oatp-proofwidgets](https://github.com/jonaprieto/oatp-proofwidgets) package.

## Roadmap

See [TODO.md](TODO.md) and the [issue tracker](https://github.com/jonaprieto/oatp/issues).

The two open tracker issues are [#2](https://github.com/jonaprieto/oatp/issues/2),
real HTTPS/streaming transport beyond the curl/wget boundary, and [#3](https://github.com/jonaprieto/oatp/issues/3),
conservative Lean goal translation plus verified ATP proof-step reconstruction.

## Binary releases

Pushing a `v*` tag runs the release workflow and publishes `otp` archives for
Linux x86_64, macOS x86_64, and macOS arm64, plus `SHA256SUMS`. The archives
contain the native binary, `LICENSE`, and this README; Lean is only needed to
build them. These stable names are ready for a later Homebrew formula:

```sh
git tag v0.2.1
git push origin v0.2.1
```

## Development

```sh
lake build OATP OATP.Properties demo proof-demo oatp tests
```

The repository follows the same separate-properties-target convention as the
other ecosystem libraries. CI builds all targets, runs the executable tests,
checks the demo, checks Lean style, and audits the properties target's axioms.

Process and HTTP output limits are checked after capture in this prototype;
large untrusted outputs therefore remain a future streaming-limit slice. The
container wrappers limit the prover process itself, but do not replace those
library-level limits. The CI matrix builds and runs a no-network identity
fixture against E, Vampire, and Metis.
Process deadlines request process-group termination through Lean's native API.
Lean 4.32's `Std.Http` is currently a low-level sans-I/O HTTP/1.1 protocol and
transport layer, not a complete HTTPS client, so OATP keeps `curl`/`wget` as
its explicit transport boundary until a suitable client API exists.
