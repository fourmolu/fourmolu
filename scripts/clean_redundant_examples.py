#!/usr/bin/env python3

from pathlib import Path

TOP = Path(__file__).resolve().parent.parent


def main():
    for four_out_hs in (TOP / "data/examples").rglob("*-four-out.hs"):
        input_name = four_out_hs.name.replace("-four-out.hs", ".hs")
        if not four_out_hs.with_name(input_name).exists():
            print(f"Removing: {four_out_hs}")
            four_out_hs.unlink()


if __name__ == "__main__":
    main()
