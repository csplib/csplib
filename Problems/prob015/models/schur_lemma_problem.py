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

from nucs.problems.problem import Problem
from nucs.propagators.propagators import ALG_AFFINE_LEQ, ALG_EXACTLY_TRUE, ALG_LEXICOGRAPHIC_LEQ
from nucs.solvers.backtrack_solver import BacktrackSolver
from nucs.statistics import get_statistics


class SchurLemmaProblem(Problem):
    """
    CSPLIB problem #15 - https://www.csplib.org/Problems/prob015/
    """

    def __init__(self, n: int) -> None:
        super().__init__([(0, 1)] * n * 3)
        for x in range(n):
            self.add_propagator(([x * 3, x * 3 + 1, x * 3 + 2], ALG_EXACTLY_TRUE, [1]))
        for x in range(n):
            for y in range(n):
                z = (x + 1) + (y + 1) - 1
                if 0 <= z < n:
                    for k in range(3):
                        self.add_propagator(([3 * x + k, 3 * y + k, 3 * z + k], ALG_AFFINE_LEQ, [1, 1, 1, 2]))
        # breaking symmetries
        self.add_propagator(
            (list(range(0, n * 3, 3)) + list(range(1, n * 3, 3)) + list(range(2, n * 3, 3)), ALG_LEXICOGRAPHIC_LEQ, [])
        )


# Run with the following command (the second run is much faster because the code has been compiled):
# NUMBA_CACHE_DIR=.numba/cache python schur_lemma_problem.py -n 20
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", type=int, default=20)
    args = parser.parse_args()
    problem = SchurLemmaProblem(args.n)
    solver = BacktrackSolver(problem)
    solver.solve_all()
    print(get_statistics(solver.statistics))