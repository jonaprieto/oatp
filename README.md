# oatp

[![CI](https://github.com/jonaprieto/oatp/actions/workflows/ci.yml/badge.svg)](https://github.com/jonaprieto/oatp/actions/workflows/ci.yml)
[![Lean 4](https://img.shields.io/badge/Lean%204-library-5f5f5f)](lean-toolchain)
[![License](https://img.shields.io/badge/license-Apache--2.0-green)](LICENSE)

ATP orchestration for Lean 4 with TPTP parsing, local and online prover runners, reproducible
artifacts, diagnostics, and explicit trust boundaries.

External ATP output is a candidate result, not a Lean proof. Kernel-checked reconstruction is
available for the supported propositional calculus.

## Quick start

```sh
lake build
lake exe demo
lake exe proof-demo
lake exe oatp --help
lake exe oatp systems
lake exe oatp repl
lake exe tests
```

`oatp repl` keeps the TPTP context, conjectures, formulas, variables, symbols, history, Lean
goals, translated problems, prover artifacts, and kernel-checked terms in one session:

```text
/to-lean p => p
/load problem.p
/snapshot
/to-tptp
/reconstruct implication-intro h exact h
/term
/run --prover eprover
/local ./my-prover -- --arg
/online --system online-vampire
/systems
/doctor
```

Use `/state` for the context drawer, `/history` for the transcript, and `--script FILE` for a
non-interactive session. `/to-lean` currently accepts the propositional TPTP fragment; terms and
quantifiers remain available for `/parse` and prover execution and return an explicit diagnostic
when a Lean signature is required.

Run a local problem or select an online system explicitly:

```sh
lake exe oatp run problem.p
lake exe oatp run --prover eprover problem.p
lake exe oatp run --prover online-vampire problem.p
```

Without `--prover`, `run` uses the first installed local prover. Set
`OATP_LOCAL_PROVERS` to control the local candidate order; online systems are opt-in.

## Provides

- pure prover, artifact, outcome, limit, and search-event models;
- Grip-backed [`tptp`](https://github.com/jonaprieto/lean-grip-tptp) parsing;
- bounded local process and HTTP transport;
- concurrent local-prover portfolios;
- SystemOnTPTP catalogue and cache;
- shared Argus option specs for the batch CLI and REPL;
- proposition-to-TPTP translation and small kernel-checked reconstruction;
- plain and ANSI terminal rendering through the TermColor stack.

The standalone CLI is available in release archives. The Lean library can be installed with:

```lean
require oatp from git
  "https://github.com/jonaprieto/oatp.git" @ "v0.6.0"
```

## Build

```sh
lake build OATP OATP.Properties demo proof-demo oatp tests
lake exe tests
```

## Related projects

[`oatp-proofwidgets`](https://github.com/jonaprieto/oatp-proofwidgets) provides optional Infoview
views. [`argus`](https://github.com/jonaprieto/lean-argus) provides typed CLI parsing.

The CLI and REPL share `OATP.Argus` resource, catalogue, and online-service option specs; their
different problem/session positionals remain frontend-specific.

For the `v0.6` migration, `RunRequest.references` is now typed as
`List OATP.ProverReference`; option records expose shared groups under `resources`, `catalogue`,
and `remote`. Legacy persisted prover names remain accepted and are rewritten with `local:` or
`online:` prefixes.

## License

Apache-2.0.
