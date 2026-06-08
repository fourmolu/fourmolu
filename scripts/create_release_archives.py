#!/usr/bin/env python3
"""
For each file in the given directory named:
    fourmolu-0.0.0-linux-x86_64

Convert it into a zip file in the same directory named:
    fourmolu-0.0.0-linux-x86_64.zip

Containing the following directory structure:
    fourmolu-0.0.0-linux-x86_64/
    └── fourmolu
"""

import argparse
import logging
import shutil
import subprocess
import tempfile
from pathlib import Path


def main():
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("files", type=Path, nargs="+")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory() as tmpdir:
        for file in args.files:
            if not (file.is_file() and file.name.startswith("fourmolu-")):
                logging.warning(f"Skipping file: {file}")
                continue

            create_archive(
                src=file,
                dest=file.parent / f"{file.name}.zip",
                tmpdir=Path(tmpdir),
            )


def create_archive(*, src: Path, dest: Path, tmpdir: Path) -> None:
    outer_dir = src.name
    (tmpdir / outer_dir).mkdir()

    src.chmod(0o755)
    src.rename(tmpdir / outer_dir / "fourmolu")

    subprocess.run(
        ["zip", "-r", dest.absolute(), outer_dir],
        cwd=tmpdir,
        stdout=subprocess.DEVNULL,
        check=True,
    )

    shutil.rmtree(tmpdir / outer_dir)
    logging.info(f"Successfully created {dest}")


if __name__ == "__main__":
    main()
