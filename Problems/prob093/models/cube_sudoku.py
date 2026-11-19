# SPDX-License-Identifier: Apache-2.0
# Category: cube_sudoku_benchmark
# Name: Cube Sudoku
# Source: Patrick Schibli / Dream Factory, Cube Sudoku Benchmark v1.0.0 (CC BY 4.0)
# Source description: https://www.dream-factory.app/cubesudoku/en/datasets
# Problem instances: https://doi.org/10.5281/zenodo.24342566

# Data
givens = (
    "0613000004091020008320590160700350000132040606280070001000006902060700"
    "3004500070100000070300600540907203056070040090606000010850000600002406"
    "0010030051000600948007900046802689000370102210058308193003480010000205"
    "0009060009510007005000000006080000500070405090253000930000000020200030"
    "6300608920018038006024500000009173001762509083000606702001692000206340"
    "800050670210635000401068090419480156"
)
# End of data

import json

import cpmpy as cp


def cube_topology():
    """Return the release's ordered surface coordinates and distinct Sudoku units."""
    index_by_coordinate = {}
    units = set()
    for axis in range(3):
        for boundary in (0, 8):
            face = []
            for row in range(9):
                indices = []
                for column in range(9):
                    point = [column, row]
                    point.insert(axis, boundary)
                    indices.append(index_by_coordinate.setdefault(tuple(point), len(index_by_coordinate)))
                face.append(indices)

            groups = [*face, *zip(*face)]
            for top in range(0, 9, 3):
                for left in range(0, 9, 3):
                    groups.append([cell for row in face[top:top + 3] for cell in row[left:left + 3]])
            units.update(tuple(sorted(group)) for group in groups)
    return tuple(index_by_coordinate), tuple(sorted(units))


def build_model(clues):
    """Build the satisfaction model and its output variables for a 386-digit puzzle."""
    coordinates, units = cube_topology()
    if len(clues) != len(coordinates) or any(digit not in "0123456789" for digit in clues):
        raise ValueError("givens must contain 386 ASCII digits, with 0 marking empty cells")

    cells = cp.intvar(1, 9, shape=len(coordinates), name="cells")
    model = cp.Model([cp.AllDifferent([cells[i] for i in unit]) for unit in units])
    model += [cells[i] == int(digit) for i, digit in enumerate(clues) if digit != "0"]
    return model, cells


model, cells = build_model(givens)
if __name__ == "__main__":
    if not model.solve(solver="ortools"):
        raise RuntimeError("No solution found for the embedded example")
    solution = {"cells": cells.value().tolist()}
    print(json.dumps(solution))
