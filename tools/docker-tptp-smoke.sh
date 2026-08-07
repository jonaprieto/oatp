#!/bin/sh
set -eu

image="${1:?usage: tools/docker-tptp-smoke.sh IMAGE [PROVER ARGS...]}"
shift
output="$(mktemp)"
trap 'rm -f "$output"' EXIT HUP INT TERM

if tools/run-tptp-docker.sh "$image" "$@" \
    < docker/tptp/fixtures/identity.p > "$output" 2>&1; then
  :
else
  status=$?
  cat "$output"
  exit "$status"
fi

cat "$output"
grep -Eq '^[#%] SZS status Theorem([[:space:]]|$)' "$output"
printf '%s\n' "Docker TPTP smoke test passed: $image"
