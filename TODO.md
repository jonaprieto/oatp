# TODO

## Next vertical slices

- [x] add a unified CLI portfolio with Argus diagnostics and live progress/table UX;
- [x] discover local provers and opt-in online SystemOnTPTP systems with aliases;
- [x] cache only the online catalogue, keyed by endpoint, with refresh/bypass flags;
- [x] add a typed first-order formula generator on top of the balanced envelope;
- [x] parse the supported first-order fragment into a semantic TPTP AST;
- [ ] add streaming multipart uploads for large proof artifacts;
- [x] add recorded SystemOnTPTP response fixtures and richer artifact metadata;
- [x] add reproducible E, Vampire, and Metis containers and no-network smoke
  fixtures;
- [ ] add `Std.Http` transport when the stable client/session API is ready;
- [x] translate one explicitly supported first-order fragment to TPTP;
- [x] translate a documented conservative Lean proposition fragment to TPTP;
- [ ] translate ATP proof steps into `OATP.Proof.Step` and keep them accepted
  only by the kernel reconstruction boundary;
- [ ] add optional Aesop portfolio orchestration;
- [x] add shared proof/search widgets for terminal and ProofWidgets4 views;
- [x] add recorded remote fixtures and no-network CI tests.

## Open issues and next work

- [ ] [#2](https://github.com/jonaprieto/oatp/issues/2): replace the curl/wget
  boundary when Lean exposes a stable HTTPS client/session API; keep bounded
  streaming and deterministic transport mocks;
- [x] [#3](https://github.com/jonaprieto/oatp/issues/3): translate a
  documented conservative Lean proposition fragment;
- [ ] extend #3 with ATP proof-step translation accepted by the kernel
  reconstruction boundary;
- [ ] add prover-specific command adapters and recorded local-process fixtures;
- [ ] add artifact export and a stable machine-readable CLI output mode.
- [ ] return per-system statuses from a batched SystemOnTPTP response instead of
  one aggregate remote artifact;
- [ ] add a catalogue fixture and an HTTP transport seam for fully offline CLI
  tests.

## Sibling-library requirements

- `lean-termcolor-widgets`: keep progress, status, and table rendering pure;
  OATP owns portfolio state and completion labels, not clocks or terminal writes;
- `lean-termcolor-terminal`: own TTY detection, dynamic-width live-region
  redraw, flushing, cursor cleanup, and non-TTY fallback;
- `lean-argus`: preserve bad-value diagnostics through optional flags and render
  help for the resolved subcommand after a parse failure.

## Non-goals for the prototype

- no custom TLS implementation;
- no remote calls during Lean compilation;
- no claim that an external SZS result is a Lean proof;
- no global Aesop rule that launches a remote prover;
- no browser widget framework duplicated inside TermColor.
