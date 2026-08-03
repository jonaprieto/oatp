#!/bin/sh
set -eu

image="${1:?usage: scripts/docker-eprover-smoke.sh IMAGE}"
shift

exec scripts/docker-tptp-smoke.sh "$image" "$@"
