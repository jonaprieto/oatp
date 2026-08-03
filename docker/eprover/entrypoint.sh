#!/bin/sh
set -eu

umask 077
problem="$(mktemp /tmp/oatp-problem.XXXXXX.p)"
trap 'rm -f "$problem"' EXIT HUP INT TERM

cat > "$problem"
exec eprover --tstp-in --tstp-out --auto "$@" "$problem"

