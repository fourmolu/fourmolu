#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

npm install
npx webpack --config webpack.config.js "$@"

# symlink worker output, for development
ln -sf ../../worker/dist/worker.js ../site/static/
