#!/usr/bin/env python3

import argparse
import json

# https://github.com/actions/runner-images?tab=readme-ov-file#available-images
# https://github.blog/changelog/2025-01-16-linux-arm64-hosted-runners-now-available-for-free-in-public-repositories-public-preview/
IMAGES = {
    "linux-x86_64": "ubuntu-24.04",
    "linux-arm64": "ubuntu-24.04-arm",
    "osx-x86_64": "macos-15-intel",
    "osx-arm64": "macos-15",
}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--is-release", action="store_true")
    args = parser.parse_args()

    target_options = ["linux-x86_64"]

    if args.is_release:
        target_options = IMAGES.keys()

    jobs = [
        {
            "target": target,
            "os": IMAGES[target],
        }
        for target in target_options
    ]
    print("jobs=" + json.dumps(jobs))

if __name__ == "__main__":
    main()
