from typing import List

from nucs.problems.problem import Problem
from nucs.propagators.propagators import ALG_AFFINE_EQ, ALG_AFFINE_LEQ


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
