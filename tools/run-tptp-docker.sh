#!/bin/sh
set -eu

image="${1:?usage: tools/run-tptp-docker.sh IMAGE [PROVER ARGS...]}"
shift

exec docker run --rm --interactive \
  --network none \
  --read-only \
  --cap-drop=ALL \
  --security-opt=no-new-privileges \
  --pids-limit=64 \
  --memory=512m \
  --cpus=1 \
  --tmpfs /tmp:rw,noexec,nosuid,size=64m \
  "$image" "$@"
