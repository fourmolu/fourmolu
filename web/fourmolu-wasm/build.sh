#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

ARGS=("$@")

listbin() {
    # https://github.com/haskell/cabal/commit/ed407b17f371c5b9ce3d40db6c939b408ef9e093
    local BIN="$(wasm32-wasi-cabal list-bin -v0 "$@")"
    case "${BIN}" in
        (*.wasm) echo "${BIN}" ;;
        (*) echo "${BIN}.wasm" ;;
    esac
}

# can't do cabal install because it'll build fourmolu too
# https://github.com/haskell/cabal/issues/8614
wasm32-wasi-cabal build exe:fourmolu-wasm "${ARGS[@]}"
mkdir -p "${HERE}/dist/"
cp "$(listbin fourmolu-wasm)" "${HERE}/dist/"

# symlink WASM output, for development
ln -sf ../../fourmolu-wasm/dist/fourmolu-wasm.wasm ../site/static/
