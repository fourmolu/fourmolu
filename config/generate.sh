#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

if command -v runghc &> /dev/null; then
    runghc -- -Wall -Werror Generate.hs
elif command -v stack &> /dev/null; then
    stack runghc -- -Wall -Werror Generate.hs
else
    echo "runghc and stack could not be found"
    exit 1
fi
