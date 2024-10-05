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
from typing import List

from nucs.problems.problem import Problem
from nucs.propagators.propagators import ALG_AFFINE_EQ, ALG_AFFINE_LEQ
from nucs.solvers.backtrack_solver import BacktrackSolver
from nucs.solvers.heuristics import first_not_instantiated_var_heuristic, max_value_dom_heuristic
from nucs.statistics import get_statistics


class KnapsackProblem(Problem):
    """
    CSPLIB problem #133 - https://www.csplib.org/Problems/prob133/
    """

    def __init__(self, weights: List[int], volumes: List[int], capacity: int) -> None:
        n = len(weights)
        super().__init__([(0, 1)] * n + [(0, sum(weights))])
        self.add_propagator((list(range(n)), ALG_AFFINE_LEQ, [*volumes, capacity]))
        self.add_propagator((list(range(n + 1)), ALG_AFFINE_EQ, [*weights, -1, 0]))
        self.weight = n


# Run with the following command (the second run is much faster because the code has been compiled):
# NUMBA_CACHE_DIR=.numba/cache python knapsack_problem.py
if __name__ == "__main__":
    problem = KnapsackProblem(
        [40, 40, 38, 38, 36, 36, 34, 34, 32, 32, 30, 30, 28, 28, 26, 26, 24, 24, 22, 22],
        [40, 40, 38, 38, 36, 36, 34, 34, 32, 32, 30, 30, 28, 28, 26, 26, 24, 24, 22, 22],
        55,
    )
    solver = BacktrackSolver(
        problem, var_heuristic=first_not_instantiated_var_heuristic, dom_heuristic=max_value_dom_heuristic
    )
    solution = solver.maximize(problem.weight)
    print(get_statistics(solver.statistics))
    print(solution)