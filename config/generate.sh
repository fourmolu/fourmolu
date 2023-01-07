#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

dhall text --file fourmolu_yaml.dhall --output ../fourmolu.yaml
dhall text --file ConfigGen.dhall --output ../src/Ormolu/Config/Gen.hs
