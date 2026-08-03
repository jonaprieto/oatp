# oatp

Lean 4 ATP orchestration with proof-artifact-first results, TPTP support, and
rich terminal output.

[![CI](https://github.com/jonaprieto/oatp/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/oatp/actions/workflows/ci.yml)
[![Lean 4](https://img.shields.io/badge/Lean-v4.32.1-blue)](lean-toolchain)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

> Prototype: the public API is intentionally small. External ATP results remain
> untrusted until a kernel-checked reconstruction path accepts them.

## What exists

- pure prover, SZS status, limits, artifact, outcome, and search-event models;
- a positioned, balanced TPTP/TSTP `fof`/`cnf` statement-envelope parser built on [grip](https://github.com/jonaprieto/lean-grip);
- bounded HTTPS requests through an argv-safe `curl` transport, with a
  post-capture response-size check;
- a pure SystemOnTPTP response normalizer for HTTP status and SZS results;
- a local argv-safe prover runner with stdin delivery, timeout cancellation, and output limits;
- a Lean metavariable snapshotter and kernel-facing candidate checker;
- a small kernel-checked propositional reconstruction calculus;
- TermColor plain and ANSI-16 event rendering;
- separate properties and executable tests.

The central trust rule is explicit: an ATP `Theorem` result is a `candidate`,
not a Lean proof. This prototype has no public verified-result constructor;
only a future kernel-checked reconstruction path can add one.

## Quick start

```sh
lake build
lake exe demo
lake exe proof-demo
lake exe tests
python3 scripts/style-check.py
python3 scripts/check-axioms.py
```

The demo shows a goal, tactic attempts, an external candidate, and the
proof-artifact trust boundary in both plain and ANSI output. The proof demo
constructs and assigns a kernel-checked `True` proof.

## Install

```lean
require oatp from git
  "https://github.com/jonaprieto/oatp.git"
  @ "main"
```

The prototype currently targets Lean 4.32.1. Pin a release or commit for
reproducible builds.

## Ecosystem

OATP keeps pure data separate from IO. This first slice uses `grip` for
byte-oriented TPTP parsing and `termcolor` for pure terminal text. The
diagnostics, terminal, widget, and `argus` integrations are intentionally
follow-up work tracked in the issue list; future editor views will target
[ProofWidgets4](https://github.com/leanprover-community/ProofWidgets4).

## Roadmap

See [TODO.md](TODO.md) and the [issue tracker](https://github.com/jonaprieto/oatp/issues).

## Development

```sh
lake build OATP OATP.Properties demo tests
```

The repository follows the same separate-properties-target convention as the
other ecosystem libraries. CI builds all targets, runs the executable tests,
checks the demo, checks Lean style, and audits the properties target's axioms.
