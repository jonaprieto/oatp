#!/bin/sh
set -eu

image="${1:?usage: scripts/docker-eprover-smoke.sh IMAGE}"
output="$(mktemp)"
trap 'rm -f "$output"' EXIT HUP INT TERM

if OATP_EPROVER_IMAGE="$image" scripts/run-eprover-docker.sh \
    < docker/eprover/fixtures/identity.p > "$output" 2>&1; then
  :
else
  status=$?
  cat "$output"
  exit "$status"
fi

cat "$output"
grep -Fq '% SZS status Theorem' "$output"
printf '%s\n' "Docker E prover smoke test passed"
