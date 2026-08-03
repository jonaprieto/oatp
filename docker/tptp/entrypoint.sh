#!/bin/sh
set -eu

umask 077
prover="${1:?missing prover command}"
shift
problem="$(mktemp /tmp/oatp-problem.XXXXXX.p)"
trap 'rm -f "$problem"' EXIT HUP INT TERM

cat > "$problem"
exec "$prover" "$@" "$problem"
