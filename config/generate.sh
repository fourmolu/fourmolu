#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${HERE}"

if [[ ! -d "${HERE}/venv" ]]; then
    python3 -m venv venv
fi

venv/bin/python -m pip install Jinja2 PyYAML
venv/bin/python generate.py
