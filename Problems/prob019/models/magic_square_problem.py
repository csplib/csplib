from typing import List

from nucs.problems.problem import Problem
from nucs.propagators.propagators import ALG_AFFINE_EQ, ALG_AFFINE_LEQ, ALG_ALLDIFFERENT


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
