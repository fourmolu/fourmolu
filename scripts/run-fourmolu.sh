#!/usr/bin/env bash
#
# Run fourmolu, assuming it's already been built.

set -eu -o pipefail

if [[ -z "${BUILD_TYPE:-}" ]]; then
    # naive detection of stack vs cabal
    if [[ -d .stack-work ]]; then
        BUILD_TYPE=stack
    else
        BUILD_TYPE=cabal
    fi
fi

case "${BUILD_TYPE}" in
    (stack) FOURMOLU='stack exec -- fourmolu' ;;
    (cabal) FOURMOLU='cabal run -- fourmolu' ;;
    (*)
        echo "Unknown build type: ${BUILD_TYPE}" >&2
        exit 1
    ;;
esac

exec $FOURMOLU "$@"
