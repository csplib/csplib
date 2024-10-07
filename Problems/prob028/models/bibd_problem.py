###############################################################################
# __   _            _____    _____
# | \ | |          / ____|  / ____|
# |  \| |  _   _  | |      | (___
# | . ` | | | | | | |       \___ \
# | |\  | | |_| | | |____   ____) |
# |_| \_|  \__,_|  \_____| |_____/
#
# Fast constraint solving in Python  - https://github.com/yangeorget/nucs
#
# Copyright 2024 - Yan Georget
###############################################################################
import argparse
from typing import List

from nucs.problems.problem import Problem
from nucs.propagators.propagators import ALG_AND, ALG_EXACTLY_TRUE, ALG_LEXICOGRAPHIC_LEQ
from nucs.solvers.backtrack_solver import BacktrackSolver
from nucs.solvers.heuristics import max_value_dom_heuristic
from nucs.statistics import get_statistics


class BIBDProblem(Problem):
    """
    CSPLIB problem #28 - https://www.csplib.org/Problems/prob028/
    """

    def __init__(self, v: int, b: int, r: int, k: int, l: int, symmetry_breaking: bool = True):
        self.v = v  # number of points/rows
        self.b = b  # number of blocks/columns
        matrix_var_nb = v * b  # number of cells in the matrix
        additional_var_nb = ((v * (v - 1)) // 2) * b
        super().__init__([(0, 1)] * (matrix_var_nb + additional_var_nb))
        # rows: counts
        for object_idx in range(0, v):
            self.add_propagator((list(range(object_idx * b, (object_idx + 1) * b)), ALG_EXACTLY_TRUE, [r]))
        # columns: counts
        for block_idx in range(0, b):
            self.add_propagator((list(range(block_idx, v * b, b)), ALG_EXACTLY_TRUE, [k]))
        # scalar products: conjunctions and counts
        conj_idx = v * b  # index of first redundant variable
        for i1 in range(0, v - 1):
            for i2 in range(i1 + 1, v):
                conj_vars = []
                for block_idx in range(0, b):
                    self.add_propagator(([i1 * b + block_idx, i2 * b + block_idx, conj_idx], ALG_AND, []))
                    conj_vars.append(conj_idx)
                    conj_idx += 1
                self.add_propagator((conj_vars, ALG_EXACTLY_TRUE, [l]))
        if symmetry_breaking:
            # lexleq on rows
            for object_idx in range(0, v - 1):
                self.add_propagator((list(range(object_idx * b, (object_idx + 2) * b)), ALG_LEXICOGRAPHIC_LEQ, []))
            # lexleq on columns
            for block_idx in range(0, b - 1):
                self.add_propagator(
                    (
                        list(range(block_idx, v * b, b)) + list(range(block_idx + 1, v * b, b)),
                        ALG_LEXICOGRAPHIC_LEQ,
                        [],
                    )
                )

    def solution_as_matrix(self, solution: List[int]) -> List[List[int]]:
        return [[solution[i * self.b + j] for j in range(0, self.b)] for i in range(0, self.v)]


# Run with the following command (the second run is much faster because the code has been compiled):
# NUMBA_CACHE_DIR=.numba/cache python bibd_problem.py -v 8 -b 14 -r 7 -k 4 -l 3 --symmetry_breaking
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", type=int)
    parser.add_argument("-b", type=int)
    parser.add_argument("-r", type=int)
    parser.add_argument("-k", type=int)
    parser.add_argument("-l", type=int)
    parser.add_argument("--symmetry_breaking", action=argparse.BooleanOptionalAction, default=True)
    args = parser.parse_args()
    problem = BIBDProblem(args.v, args.b, args.r, args.k, args.l, args.symmetry_breaking)
    solver = BacktrackSolver(problem, dom_heuristic=max_value_dom_heuristic)
    solver.solve_one()
    print(get_statistics(solver.statistics))