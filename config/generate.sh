#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

RUNGHC=runghc
if command -v stack &> /dev/null; then
  RUNGHC="stack runghc"
fi

${RUNGHC} -- -Wall -Werror Generate.hs
