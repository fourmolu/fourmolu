#!/usr/bin/env python3

import argparse
import itertools
import json
import subprocess
from pathlib import Path
from typing import Any, Mapping

TOP = Path(__file__).resolve().parent.parent


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(required=True)

    parser_validate_changelog = subparsers.add_parser("validate-changelog")
    parser_validate_changelog.add_argument("--fourmolu-version", required=True)
    parser_validate_changelog.set_defaults(
        run=lambda args: validate_changelog(
            version=args.fourmolu_version,
        )
    )

    parser_generate = subparsers.add_parser("generate")
    parser_generate.add_argument("--output", "-o", type=Path)
    parser_generate.add_argument("--fourmolu-version", required=True)
    parser_generate.add_argument("--bin.linux-x86_64", type=Path, required=True)
    parser_generate.add_argument("--bin.linux-aarch64", type=Path, required=True)
    parser_generate.add_argument("--bin.macos-x86_64", type=Path, required=True)
    parser_generate.add_argument("--bin.macos-aarch64", type=Path, required=True)
    parser_generate.set_defaults(
        run=lambda args: generate(
            output=args.output,
            version=args.fourmolu_version,
            binaries={
                "linux-x86_64": getattr(args, "bin.linux_x86_64"),
                "linux-aarch64": getattr(args, "bin.linux_aarch64"),
                "macos-x86_64": getattr(args, "bin.macos_x86_64"),
                "macos-aarch64": getattr(args, "bin.macos_aarch64"),
            },
        )
    )

    args = parser.parse_args()
    args.run(args)


def validate_changelog(version: str) -> None:
    _ = get_changelog_for(version)


def get_changelog_for(version: str) -> str:
    changelog = (TOP / "CHANGELOG.md").read_text().splitlines()
    if changelog[0] != f"## Fourmolu {version}":
        raise Exception(
            f"""
            CHANGELOG doesn't look updated.
            Expected version: {version!r}
            Got header: {changelog[0]!r}
            """
        )
    return "\n".join(
        itertools.takewhile(
            lambda line: not line.startswith("## Fourmolu"),
            changelog[1:],
        )
    )


def generate(output: Path | None, version: str, binaries: Mapping[str, Path]) -> None:
    release_notes = [
        get_changelog_for(version),
        "## [dotslash](https://dotslash-cli.com/docs/) config",
        "```json",
        "#!/usr/bin/env dotslash",
        json.dumps(
            get_dotslash_config(version, binaries),
            indent=2,
        ),
        "```",
    ]

    write = output.write_text if output else print
    write("\n".join(release_notes))


BASE_DOWNLOAD_URL = "https://github.com/fourmolu/fourmolu/releases/download"


def get_dotslash_config(version: str, binaries: Mapping[str, Path]) -> Any:
    return {
        "name": f"fourmolu-{version}",
        "platforms": {
            name: {
                "size": bin_path.stat().st_size,
                "hash": "sha256",
                "digest": get_digest(bin_path),
                "path": "fourmolu",
                "providers": [
                    {"url": f"{BASE_DOWNLOAD_URL}/v{version}/{bin_path.name}"},
                ],
            }
            for name, bin_path in binaries.items()
        },
    }


def get_digest(bin_path: Path) -> str:
    proc = subprocess.run(
        ["shasum", "-a", "256", bin_path],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    return proc.stdout.split()[0]


if __name__ == "__main__":
    main()
