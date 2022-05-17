# pyright: strict, reportUnknownMemberType=false

from __future__ import annotations

import itertools
import json
import logging
import os
import requests
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.DEBUG)


def main():
    gh_token = os.environ["gh_token"]
    version = os.environ["version"]
    bindir = os.environ["bindir"]
    repo = os.environ["GITHUB_REPOSITORY"]
    sha = os.environ["GITHUB_SHA"]

    version_name = f"v{version}"

    # ensure release files exist
    gh_release_files = [
        Path(bindir) / f"fourmolu-{version}-linux-x86_64",
        Path(bindir) / f"fourmolu-{version}-osx-x86_64",
    ]
    all_files = gh_release_files
    for file in all_files:
        if not file.exists():
            raise Exception(f"File does not exist: {file}")

    file_paths = [file.as_posix() for file in all_files]
    logger.info(f"Creating release {version_name} with files: {file_paths}")

    # check + parse CHANGELOG
    changelog = Path("CHANGELOG.md").read_text()
    if not changelog.startswith(f"## Fourmolu {version}"):
        raise Exception("CHANGELOG doesn't look updated")
    version_changes = get_version_changes(changelog)

    create_github_release(
        repo=repo,
        token=gh_token,
        sha=sha,
        version_name=version_name,
        version_changes=version_changes,
        files=gh_release_files,
    )

    # TODO: upload to hackage?

    logger.info(f"Released fourmolu {version_name}!")


def get_version_changes(changelog: str) -> str:
    lines = changelog.split("\n")

    # skip initial '## Fourmolu X.Y.Z' line
    lines = lines[1:]

    # take lines until the next '## Fourmolu X.Y.Z' line
    lines = itertools.takewhile(lambda line: not line.startswith("## Fourmolu "), lines)

    return "\n".join(lines)


def create_github_release(
    *,
    repo: str,
    token: str,
    sha: str,
    version_name: str,
    version_changes: str,
    files: list[Path],
):
    session = init_session()
    session.headers["Accept"] = "application/vnd.github.v3+json"
    session.headers["Authorization"] = f"token {token}"
    session.headers["User-Agent"] = repo

    payload = {
        "tag_name": version_name,
        "target_commitish": sha,
        "name": version_name,
        "body": version_changes,
    }
    logger.debug(f"Creating release with: {json.dumps(payload)}")

    create_resp = session.post(
        f"https://api.github.com/repos/{repo}/releases",
        json=payload,
    )

    # upload_url is in the format: "https://...{?name,label}"
    upload_url = create_resp.json()["upload_url"]
    upload_url = upload_url.replace("{?name,label}", "")

    for file in files:
        logger.debug(f"Uploading asset: {file}")
        with file.open("rb") as f:
            session.post(
                upload_url,
                headers={"Content-Type": "application/octet-stream"},
                params={"name": file.name},
                data=f,
            )


def init_session() -> requests.Session:
    session = requests.Session()

    def _check_status(r: requests.Response, *args: Any, **kwargs: Any):
        r.raise_for_status()

    # https://github.com/python/typeshed/issues/7776
    session.hooks["response"].append(  # pyright: ignore[reportFunctionMemberAccess]
        _check_status,
    )

    return session


if __name__ == "__main__":
    main()
