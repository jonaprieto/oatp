#!/usr/bin/env python3
"""Fail if the properties target grows an unexpected axiom dependency."""

import re
import subprocess
import sys

allowed = {"propext", "Classical.choice", "Quot.sound"}
result = subprocess.run(
    ["lake", "build", "OATP.Properties"],
    text=True,
    capture_output=True,
)
sys.stdout.write(result.stdout)
sys.stderr.write(result.stderr)
if result.returncode:
    raise SystemExit(result.returncode)

current = None
block = []
failures = []
for line in (result.stdout + result.stderr).splitlines():
    match = re.search(r"'OATP\.Properties\.([^']+)' depends on axioms:", line)
    if match:
        current = match.group(1)
        block = [line.split("depends on axioms:", 1)[1]]
    elif current:
        block.append(line)
    if current and "]" in "".join(block):
        axioms = set(re.findall(r"[A-Za-z][A-Za-z0-9.]*", "".join(block)))
        unexpected = axioms - allowed
        if unexpected:
            failures.append((current, unexpected))
        current = None
        block = []

if failures:
    for name, axioms in failures:
        print(f"unexpected axioms in {name}: {sorted(axioms)}", file=sys.stderr)
    raise SystemExit(1)
