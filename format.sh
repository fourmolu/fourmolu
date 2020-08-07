#!/bin/sh

set -e

export LANG="C.UTF-8"

cabal format

fourmolu -i $(find app -type f -name "*.hs")
fourmolu -i $(find src -type f \( -name "*.hs" -o -name "*.hs-boot" \))
fourmolu -i $(find tests -type f -name "*.hs")
