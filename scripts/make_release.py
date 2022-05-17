# pyright: strict, reportUnknownMemberType=false

from __future__ import annotations

import itertools
import logging
import os
import re
import requests
import sys
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.DEBUG)


def main(file_paths: list[Path]):
    logger.info(f"Creating release with files: {[str(f) for f in file_paths]}")

    token = os.environ["token"]
    version = os.environ["version"]
    repo = os.environ["GITHUB_REPOSITORY"]
    sha = os.environ["GITHUB_SHA"]

    version_name = f"v{version}"

    # check files
    files = [(path.name, path.read_bytes()) for path in file_paths]
    file_names = [name for name, _ in files]
    if len(set(file_names)) != len(file_names):
        raise Exception(f"Found multiple files with the same name: {file_names}")

    # check + parse CHANGELOG
    changelog = Path("CHANGELOG.md").read_text()
    if not changelog.startswith(f"## Fourmolu {version}"):
        raise Exception("CHANGELOG doesn't look updated")
    version_changes = get_version_changes(changelog)

    session = requests.Session()
    session.headers["Accept"] = "application/vnd.github.v3+json"
    session.headers["Authorization"] = f"token {token}"
    session.headers["User-Agent"] = repo

    def _check_status(r: requests.Response, *args: Any, **kwargs: Any):
        r.raise_for_status()

    session.hooks["response"].append(_check_status)

    create_resp = session.post(
        f"https://api.github.com/repos/{repo}/releases",
        json={
            "tag_name": version_name,
            "target_commitish": sha,
            "name": version_name,
            "body": version_changes,
        },
    )

    # upload_url is in the format: "https://...{?name,label}"
    upload_url = create_resp.json()["upload_url"]
    upload_url = re.sub(r"{?name,label}$", r"", upload_url)

    for file_name, file_content in files:
        session.post(
            upload_url,
            params={"name": file_name},
            body=file_content,
        )

    logger.info(f"Released fourmolu {version_name}!")


def get_version_changes(changelog: str) -> str:
    lines = changelog.split("\n")

    # skip initial '## Fourmolu X.Y.Z' line
    lines = lines[1:]

    # take lines until the next '## Fourmolu X.Y.Z' line
    lines = itertools.takewhile(lambda line: line.startswith("## Fourmolu "), lines)

    return "\n".join(lines)


if __name__ == "__main__":
    files = [Path(f) for f in sys.argv[1:]]
    main(files)
