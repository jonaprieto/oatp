# TODO

## Next vertical slices

- [x] add semantic TPTP/TSTP AST modules on top of the balanced envelope;
- [ ] add streaming multipart uploads for large proof artifacts;
- [ ] add recorded SystemOnTPTP response fixtures and richer artifact metadata;
- [ ] add `Std.Http` transport when the stable client/session API is ready;
- [ ] add prover-specific command adapters and recorded local-process fixtures;
- [x] translate one explicitly supported first-order fragment to TPTP;
- [ ] translate ATP proof steps into `OATP.Proof.Step`;
- [ ] add optional Aesop portfolio orchestration;
- [ ] add shared proof/search widgets for terminal and ProofWidgets4 views;
- [ ] add recorded remote fixtures and no-network CI tests.

## Non-goals for the prototype

- no custom TLS implementation;
- no remote calls during Lean compilation;
- no claim that an external SZS result is a Lean proof;
- no global Aesop rule that launches a remote prover;
- no browser widget framework duplicated inside TermColor.
