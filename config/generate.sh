#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

runghc -Wall -Werror Generate.hs
