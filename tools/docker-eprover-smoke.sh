#!/bin/sh
set -eu

image="${1:?usage: tools/docker-eprover-smoke.sh IMAGE}"
shift

exec tools/docker-tptp-smoke.sh "$image" "$@"
