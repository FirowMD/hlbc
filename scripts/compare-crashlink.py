#!/usr/bin/env python3
"""Compare readable hlbc output with crashlink on shared feature fixtures."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys


CATEGORIES = {
    "arrays": "Arrays",
    "enums": "Enums",
    "loops": "LoopWhile",
    "switches": "Switch",
    "strings": "StringInterp",
}


def metrics(category: str, source: str) -> dict[str, int]:
    lowered = source.lower()
    state_machines = source.count("__hl_state")
    unhandled = sum(
        lowered.count(marker)
        for marker in ("unsupported opcode", "unliftedopcode", "untranslatedopcode")
    )
    compiler_temporaries = len(set(re.findall(r"\b__hl_[A-Za-z0-9_]*", source)))
    non_empty_lines = sum(bool(line.strip()) for line in source.splitlines())

    if category == "arrays":
        constructs = len(re.findall(r"=\s*(?:\([^;]*\)\s*)?\[[^\]]*\]", source))
    elif category == "enums":
        constructs = (
            len(re.findall(r"(?m)^\s*enum\s+", source))
            + len(re.findall(r"case\s+\w+\s*(?:\([^)]*\))?\s*:", source))
            + len(re.findall(r"\bswitch\s*\(", source))
        )
    elif category == "loops":
        constructs = len(re.findall(r"\b(?:while|for)\s*\(", source))
    elif category == "switches":
        constructs = len(re.findall(r"\bswitch\s*\(", source))
    else:
        constructs = len(re.findall(r"\$(?:\{[^}]+\}|[A-Za-z_]\w*)", source))
    if state_machines:
        constructs = 0

    return {
        "structured_constructs": constructs,
        "state_machines": state_machines,
        "unhandled_markers": unhandled,
        "compiler_temporaries": compiler_temporaries,
        "non_empty_lines": non_empty_lines,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--crashlink", required=True, type=pathlib.Path)
    parser.add_argument("--fixtures", required=True, type=pathlib.Path)
    parser.add_argument("--hlbc-output", required=True, type=pathlib.Path)
    args = parser.parse_args()

    sys.path.insert(0, str(args.crashlink.resolve()))
    from crashlink import Bytecode, decomp

    comparisons = []
    for category, fixture in CATEGORIES.items():
        bytecode = Bytecode.from_path(str(args.fixtures / f"{fixture}.hl"))
        crashlink_text = decomp.IRClass(
            bytecode, bytecode.get_test_obj(fixture)
        ).pseudo()
        hlbc_text = (
            args.hlbc_output / fixture / "readable" / f"{fixture}.hx"
        ).read_text(encoding="utf-8")
        hlbc_metrics = metrics(category, hlbc_text)
        crashlink_metrics = metrics(category, crashlink_text)
        equivalent = (
            hlbc_metrics["structured_constructs"]
            >= crashlink_metrics["structured_constructs"]
            and hlbc_metrics["state_machines"] <= crashlink_metrics["state_machines"]
            and hlbc_metrics["unhandled_markers"]
            <= crashlink_metrics["unhandled_markers"]
        )
        comparisons.append(
            {
                "category": category,
                "fixture": fixture,
                "hlbc": hlbc_metrics,
                "crashlink": crashlink_metrics,
                "equivalent_or_better": equivalent,
                "message": (
                    "structured constructs and fallback markers are equivalent or better"
                    if equivalent
                    else "hlbc has fewer structured constructs or more fallback markers"
                ),
            }
        )

    print(
        json.dumps(
            {
                "crashlink_supplied": True,
                "crashlink_available": True,
                "comparisons": comparisons,
                "passed": all(item["equivalent_or_better"] for item in comparisons),
                "error": None,
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
