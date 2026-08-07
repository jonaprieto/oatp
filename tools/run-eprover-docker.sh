#!/bin/sh
set -eu

image="${OATP_EPROVER_IMAGE:-oatp/eprover:bookworm-2.6}"

exec tools/run-tptp-docker.sh "$image" "$@"
