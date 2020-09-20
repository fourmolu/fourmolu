#!/bin/sh

set -e

cabal format

fourmolu -i \
    $(find app -type f -name "*.hs") \
    $(find src -type f \( -name "*.hs" -o -name "*.hs-boot" \)) \
    $(find tests -type f -name "*.hs") \
