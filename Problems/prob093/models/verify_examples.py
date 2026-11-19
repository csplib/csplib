# SPDX-License-Identifier: Apache-2.0
from collections import Counter
import json
from pathlib import Path

import cpmpy as cp
from cpmpy.solvers.solver_interface import ExitStatus

from cube_sudoku import build_model, cube_topology


def main():
    data = Path(__file__).resolve().parents[1] / "data"
    topology = json.loads((data / "topology.json").read_text(encoding="utf-8"))
    coordinates, units = cube_topology()
    if coordinates != tuple(map(tuple, topology["coordinates_xyz"])):
        raise ValueError("Cell order differs from the published topology")
    if len(coordinates) != 386 or len(units) != 150:
        raise ValueError("Unexpected numbers of cells or Sudoku units")
    if set(units) != {tuple(sorted(unit)) for unit in topology["units"]}:
        raise ValueError("Sudoku units differ from the published topology")

    examples = json.loads((data / "examples.json").read_text(encoding="utf-8"))
    levels = ("easy", "medium", "hard", "expert", "master")
    if Counter(example["app_difficulty"] for example in examples) != dict.fromkeys(levels, 1):
        raise ValueError("Expected one example per app difficulty")

    for example in examples:
        puzzle_id = example["puzzle_id"]
        expected = [int(digit) for digit in example["solution"]]
        model, cells = build_model(example["givens"])
        if not model.solve(solver="ortools", time_limit=10, num_search_workers=1):
            raise RuntimeError(f"{puzzle_id}: {model.status().exitstatus.name} while solving")
        if cells.value().tolist() != expected:
            raise ValueError(f"{puzzle_id}: answer differs from the released solution")

        model += cp.any(cells != expected)
        model.solve(solver="ortools", time_limit=10, num_search_workers=1)
        if model.status().exitstatus != ExitStatus.UNSATISFIABLE:
            raise RuntimeError(f"{puzzle_id}: uniqueness check returned {model.status().exitstatus.name}")
        print(f"{example['app_difficulty']:>6}  {puzzle_id}  matches release and is unique")


if __name__ == "__main__":
    main()
