# OATP architecture

OATP is a proof-artifact-first orchestration layer. It turns Lean goals or
TPTP problems into bounded local or remote ATP runs, but never treats an ATP
answer as a Lean proof by itself.

## Component boundaries

```mermaid
flowchart LR
  user["User / Lean tactic"] --> cli["examples/Cli.lean\nArgus CLI"]
  user --> facade["OATP.lean\npublic facade"]

  subgraph pure["Pure and inspectable models"]
    core["OATP.Core\nProblem · Artifact · SZSStatus"]
    events["OATP.Events\nSearchEvent · GoalSnapshot"]
    tptp["OATP.TPTP\nlean-grip-tptp adapter"]
    term["OATP.Term\nplain / ANSI event rendering"]
  end

  subgraph boundaries["Execution and trust boundaries"]
    local["OATP.Process\nargv-safe local process"]
    http["OATP.Http\ncurl / wget transport"]
    system["OATP.SystemOnTPTP\nform adapter + response parser"]
    translate["OATP.Translate\nconservative Lean → TPTP"]
    lean["OATP.Lean\nmeta snapshot + kernel check"]
    proof["OATP.Proof\nsmall reconstruction calculus"]
  end

  cli --> local
  cli --> system
  cli --> term
  system --> http
  system --> core
  local --> core
  translate --> tptp
  translate --> lean
  proof --> lean
  events --> core
  term --> events
  facade --> core
  facade --> boundaries

  external["External ATP / HTTP"] --> local
  external --> http
  local --> artifact["Untrusted Artifact"]
  http --> artifact
  artifact -.-> proof
  proof --> checked["CheckedProof\naccepted by Lean kernel"]

  classDef pure fill:#e8f5e9,stroke:#2e7d32,color:#102a13
  classDef boundary fill:#fff3e0,stroke:#ef6c00,color:#3e2100
  classDef trust fill:#fce4ec,stroke:#ad1457,color:#3b071c
  class core,events,tptp,term pure
  class local,http,system,translate,lean,proof boundary
  class artifact,checked trust
```

The dashed edge is intentional: reconstruction consumes an external result
only as data. `OATP.Proof.reconstruct` builds a candidate expression and
`OATP.Lean.checkAndAssign` checks it against the metavariable before assigning
anything.

## Runtime flows

```mermaid
sequenceDiagram
  participant U as User / CLI
  participant A as Argus
  participant B as OATP portfolio
  participant E as Local ATPs
  participant S as SystemOnTPTP
  participant R as Artifact

  U->>A: run [--prover REF]...
  A->>B: typed Problem + local/online attempts
  par local portfolio
    B->>E: spawn argv, write problem on stdin
    E-->>B: stdout, stderr, exit status
  and explicit online portfolio
    B->>S: catalogue lookup, then one bounded batch request
    S-->>B: HTTP response
  end
  B->>R: normalize each result and preserve output
  R-->>U: candidate artifact and exit status
```

The current runtime limits are deliberately conservative:

- local process groups receive timeout termination requests;
- local and HTTP response limits are checked after capture;
- HTTP request bodies are rejected before transport when oversized;
- true streaming upload/response cancellation remains a future transport
  slice.

## Module ownership

| Area | Owner | Boundary |
| --- | --- | --- |
| Domain data | `OATP.Core` | no IO, no terminal, no proof claims |
| Search events | `OATP.Events` | renderer-independent data |
| TPTP syntax | `lean-grip-tptp` via `OATP.TPTP` | parsing and formula rendering |
| Lean goals | `OATP.Translate` | supported fragment only; reject the rest |
| Kernel safety | `OATP.Lean`, `OATP.Proof` | candidate terms checked by Lean |
| Local ATPs | `OATP.Process` | argv, stdin, timeout, output limits |
| Portfolio scheduling | `OATP.Portfolio` | concurrent attempts, completion order, batch remote job |
| Remote ATPs | `OATP.Http`, `OATP.SystemOnTPTP` | curl/wget, catalogue, form requests, response parsing |
| Terminal output | `OATP.Term` + TermColor libraries | pure rendering vs TTY IO |
| CLI | `examples/Cli.lean` + Argus | parsing, diagnostics, orchestration |

Sibling-library responsibilities stay narrow: Argus owns typed command parsing
and usage diagnostics; `termcolor-widgets` owns pure progress frames;
`termcolor-terminal` owns TTY detection, redraw, flushing, and cursor cleanup.

## Public entry points

- `import OATP.Core` for pure domain values;
- `import OATP.TPTP` for TPTP parsing and construction;
- `import OATP.Process` or `import OATP.Http` for execution boundaries;
- `import OATP.Lean` and `import OATP.Proof` for kernel-facing integrations;
- `import OATP` for the complete library facade;
- `lake exe oatp` for the standalone CLI. `oatp run problem.p` is local-only by
  default; network systems are explicit `--prover online-*` references.

The umbrella module is intentionally a convenience import. Internal modules
remain separately usable so consumers do not need to depend on the CLI.

## Deliberate next seams

1. Replace the command-backed HTTP edge only when Lean exposes a stable HTTPS
   client/session API.
2. Add streaming transport without changing `Problem`, `Artifact`, or the
   kernel trust boundary.
3. Parse ATP proof steps into `OATP.Proof.Step`; do not widen reconstruction
   until every accepted step remains kernel-checked.
4. Add stable JSON/artifact export at the CLI edge, not to the pure core.
