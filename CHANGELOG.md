# Changelog

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
