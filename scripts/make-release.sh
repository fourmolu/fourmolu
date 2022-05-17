#!/usr/bin/env bash

set -euxo pipefail
HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ ! -d "${HERE}/.venv" ]]; then
    python3 -m venv "${HERE}/.venv"
    "${HERE}/.venv/bin/pip" install requests
fi

exec "${HERE}/.venv/bin/python3" "${HERE}/make_release.py" "$@"
