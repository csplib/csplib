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
from nucs.propagators.propagators import ALG_AND, ALG_EXACTLY_TRUE, ALG_LEXICOGRAPHIC_LEQ


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
