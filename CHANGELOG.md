# Changelog

## 0.7.9 — 2026-09-09

- Remove private-repository instructions and references from the public README.

## 0.7.8 — 2026-09-08

- Refresh first-party dependencies for the Lean 4.33.1 ecosystem releases.

## 0.7.7 — 2026-08-13

- Pin `grip-json` to its newest release and document the direct relationship.

## 0.7.6 — 2026-08-13

- Pin every first-party dependency to its newest released tag.
- Adapt JSON configuration parsing to the standalone `grip-json` package.

## 0.7.5 — 2026-08-13

- Use a local error transformer for recursive Lean formula translation.

## 0.7.4 — 2026-08-13

- Publish the dependency-graph README cleanup.

## 0.7.2 — 2026-08-12

- Adopt Lean v4.33.0 and precommit-lean v0.1.6.

## 0.7.1 — 2026-08-12

- expose effective configuration through the CLI and REPL;
- add deterministic drawer toggles and improve state navigation;
- clarify indexed context editing and empty state sections.

## 0.7.0

- add indexed context editing and deterministic state-drawer toggling;
- add `all` and `first-success` prover run strategies;
- collapse `/help` history entries while keeping full output expandable;
- preserve local and online prover artifacts for each request;
- clarify run status, cached online prover information, and REPL workflow.

## 0.6.0

- use published Git dependencies for Argus `v0.5.0` and TermColor REPL
  `v0.8.1`;
- make OATP application bindings consume typed `KeyContext` values and derive
  displayed aliases from binding specifications;
- keep the live run drawer open while the main input remains focused;
- make command/help completion cover every command and `/help /COMMAND` form;
- keep transcript command text at command-level highlighting instead of
  coloring prose as TPTP roles/formats;
- add executable, help, keymap-conflict, and run-drawer regression gates.

## 0.5.1

- retain the prior release notes in repository history.
