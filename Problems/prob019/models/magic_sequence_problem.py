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
from nucs.propagators.propagators import ALG_AFFINE_EQ, ALG_COUNT_EQ
from nucs.solvers.backtrack_solver import BacktrackSolver
from nucs.solvers.heuristics import last_not_instantiated_var_heuristic, min_value_dom_heuristic
from nucs.statistics import get_statistics


class MagicSequenceProblem(Problem):
    """
    Find a sequence x_0, ... x_n-1
    such that each x_i is the number of occurences of i in the sequence.

    CSPLIB problem #19 - https://www.csplib.org/Problems/prob019/
    """

    def __init__(self, n: int):
        self.n = n
        super().__init__([(0, n)] * n)
        for i in range(n):
            self.add_propagator((list(range(n)) + [i], ALG_COUNT_EQ, [i]))
        # redundant constraints
        self.add_propagator((list(range(n)), ALG_AFFINE_EQ, [1] * n + [n]))
        self.add_propagator((list(range(n)), ALG_AFFINE_EQ, list(range(n)) + [n]))


# Run with the following command (the second run is much faster because the code has been compiled):
# NUMBA_CACHE_DIR=.numba/cache python magic_sequence_problem.py -n 100
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", type=int, default=100)
    args = parser.parse_args()
    problem = MagicSequenceProblem(args.n)
    solver = BacktrackSolver(
        problem, var_heuristic=last_not_instantiated_var_heuristic, dom_heuristic=min_value_dom_heuristic
    )
    solver.solve_all()
    print(get_statistics(solver.statistics))