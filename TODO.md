# TODO

## Next vertical slices

- [ ] replace the line-envelope parser with full TPTP/TSTP syntax modules;
- [ ] add URL-encoded and multipart form encoders;
- [ ] parse and normalize SystemOnTPTP responses into artifacts;
- [ ] add `Std.Http` transport when the stable client/session API is ready;
- [ ] add local prover process backends with typed limits and cancellation;
- [ ] add Lean goal extraction for one explicitly supported first-order fragment;
- [ ] reconstruct and kernel-check one proof calculus;
- [ ] add optional Aesop portfolio orchestration;
- [ ] add shared proof/search widgets for terminal and ProofWidgets4 views;
- [ ] add recorded remote fixtures and no-network CI tests.

## Non-goals for the prototype

- no custom TLS implementation;
- no remote calls during Lean compilation;
- no claim that an external SZS result is a Lean proof;
- no global Aesop rule that launches a remote prover;
- no browser widget framework duplicated inside TermColor.
