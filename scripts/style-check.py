#!/usr/bin/env python3
"""Mechanical Lean style gate: 100 columns, no trailing whitespace/tabs."""

import shutil
import subprocess
import sys

MAX = 100
if shutil.which("rg"):
    files = subprocess.check_output(["rg", "--files", "-g", "*.lean"], text=True).split()
else:
    files = subprocess.check_output(["git", "ls-files", "*.lean"], text=True).split()
violations = []
for path in files:
    with open(path, encoding="utf-8") as source:
        for line_number, line in enumerate(source, 1):
            line = line.rstrip("\n")
            if len(line) > MAX:
                violations.append(f"{path}:{line_number}: line is {len(line)} cols (>{MAX})")
            if line != line.rstrip():
                violations.append(f"{path}:{line_number}: trailing whitespace")
            if "\t" in line:
                violations.append(f"{path}:{line_number}: tab character")

for violation in violations:
    print(violation)
print(f"{'FAIL' if violations else 'OK'}: {len(files)} Lean files, {len(violations)} violations")
sys.exit(1 if violations else 0)
