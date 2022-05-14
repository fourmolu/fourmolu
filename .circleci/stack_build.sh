#!/usr/bin/env bash

set -eux -o pipefail

ARGS=(
    # always use development mode, to enable -Werror
    --flag fourmolu:dev
)

stack build "${ARGS[@]}" "$@"
