#!/usr/bin/env bash

set -eux -o pipefail

ARGS=(
    # always use development mode, to enable -Werror
    --flag fourmolu:dev
    # turn on fixity-th, for production build
    --flag fourmolu:fixity-th
)

stack build "${ARGS[@]}" "$@"
