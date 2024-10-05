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
from nucs.propagators.propagators import ALG_AFFINE_EQ, ALG_AFFINE_LEQ, ALG_ALLDIFFERENT
from nucs.solvers.backtrack_solver import BacktrackSolver
from nucs.solvers.heuristics import smallest_domain_var_heuristic, max_value_dom_heuristic
from nucs.statistics import get_statistics


class MagicSquareProblem(Problem):
    """
    A simple model for magic squares.

    CSPLIB problem #19 - https://www.csplib.org/Problems/prob019/
    """

    def __init__(self, n: int, symmetry_breaking: bool = True):
        self.n = n
        self.m = (n**2 - 1) * n / 2
        super().__init__([(0, n**2 - 1)] * n**2)
        for i in range(n):
            self.add_propagator((self.row(i), ALG_AFFINE_EQ, [1] * n + [self.m]))
            self.add_propagator((self.column(i), ALG_AFFINE_EQ, [1] * n + [self.m]))
        self.add_propagator((self.first_diag(), ALG_AFFINE_EQ, [1] * n + [self.m]))
        self.add_propagator((self.second_diag(), ALG_AFFINE_EQ, [1] * n + [self.m]))
        self.add_propagator((list(range(n**2)), ALG_ALLDIFFERENT, []))
        if symmetry_breaking:
            top_left = self.first_diag()[0]
            bottom_right = self.first_diag()[-1]
            top_right = self.second_diag()[0]
            bottom_left = self.second_diag()[-1]
            self.add_propagator(([top_left, top_right], ALG_AFFINE_LEQ, [1, -1, -1]))
            self.add_propagator(([top_left, bottom_left], ALG_AFFINE_LEQ, [1, -1, -1]))
            self.add_propagator(([top_left, bottom_right], ALG_AFFINE_LEQ, [1, -1, -1]))
            self.add_propagator(([top_right, bottom_left], ALG_AFFINE_LEQ, [1, -1, -1]))

    def row(self, i: int) -> List[int]:
        return list(range(0 + i * self.n, self.n + i * self.n))

    def column(self, j: int) -> List[int]:
        return list(range(j, self.n**2, self.n))

    def first_diag(self) -> List[int]:
        return list(range(0, self.n**2, self.n + 1))

    def second_diag(self) -> List[int]:
        return list(range(self.n**2 - self.n, 0, 1 - self.n))


# Run with the following command (the second run is much faster because the code has been compiled):
# NUMBA_CACHE_DIR=.numba/cache python magic_square_problem.py -n 4 --symmetry_breaking
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", type=int)
    parser.add_argument("--symmetry_breaking", action=argparse.BooleanOptionalAction, default=True)
    args = parser.parse_args()
    problem = MagicSquareProblem(args.n, args.symmetry_breaking)
    solver = BacktrackSolver(
        problem, var_heuristic=smallest_domain_var_heuristic, dom_heuristic=max_value_dom_heuristic
    )
    solver.solve_all()
    print(get_statistics(solver.statistics))