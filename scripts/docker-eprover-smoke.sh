#!/bin/sh
set -eu

image="${1:?usage: scripts/docker-eprover-smoke.sh IMAGE}"
output="$(mktemp)"
trap 'rm -f "$output"' EXIT HUP INT TERM

OATP_EPROVER_IMAGE="$image" scripts/run-eprover-docker.sh \
  < docker/eprover/fixtures/identity.p > "$output"

grep -Fq '% SZS status Theorem' "$output"
printf '%s\n' "Docker E prover smoke test passed"

