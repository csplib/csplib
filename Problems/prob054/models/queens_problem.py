from typing import List

from nucs.problems.problem import Problem
from nucs.propagators.propagators import ALG_ALLDIFFERENT


class QueensProblem(Problem):
    """
    A simple model for the n-queens problem.

    CSPLIB problem #54 - https://www.csplib.org/Problems/prob054/
    """

    def __init__(self, n: int):
        super().__init__(
            [(0, n - 1)] * n,
            list(range(n)) * 3,
            [0] * n + list(range(n)) + list(range(0, -n, -1)),
        )
        self.add_propagator((list(range(n)), ALG_ALLDIFFERENT, []))
        self.add_propagator((list(range(n, 2 * n)), ALG_ALLDIFFERENT, []))
        self.add_propagator((list(range(2 * n, 3 * n)), ALG_ALLDIFFERENT, []))

    def solution_as_matrix(self, solution: List[int]) -> List[List[str]]:
        n = len(solution)
        return [([" "] * i + ["X"] + [" "] * (n - i - 1)) for i in range(n)]
