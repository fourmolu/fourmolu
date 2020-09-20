#!/bin/sh

set -e

cabal format

cabal run fourmolu -- -i $(find app -type f -name "*.hs")
cabal run fourmolu -- -i $(find src -type f \( -name "*.hs" -o -name "*.hs-boot" \))
cabal run fourmolu -- -i $(find tests -type f -name "*.hs")
