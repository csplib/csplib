"""
PyCSP3 Model (see pycsp.org)

Examples:
  python Molnar.py -data=[5,4]
"""

from pycsp3 import *

from pycsp3.classes.entities import TypeNode

k, d = data


def determinant_terms(t):
    if len(t) == 2:
        return [t[0][0] * t[1][1], -(t[0][1] * t[1][0])]
    subterms = ([determinant_terms([[v for j, v in enumerate(row) if j != i] for row in t[1:]]) for i in range(len(t))])
    return [t[0][i] * sub if i % 2 == 0 else -(t[0][i] * sub) for i in range(len(t)) for sub in subterms[i]]


def determinant(t):
    terms = determinant_terms(t)
    # we extract coeffs from terms for posting a simpler Sum constraint later
    terms = [(term.sons[0], -1) if term.type == TypeNode.NEG else (term, 1) for term in terms]
    return [t for t, _ in terms] * [c for _, c in terms]


# x[i][j] is the value of the matrix at row i and column j
x = VarArray(size=[k, k], dom=range(2, d + 1))

# y[i][j] is the square of the value of the matrix x at row i and column j
y = VarArray(size=[k, k], dom=range(4, d * d + 1))

satisfy(
    # computing y
    [y[i][j] == x[i][j] * x[i][j] for i in range(k) for j in range(k)],

    determinant(x) == 1,

    determinant(y) == 1,

    # tag(symmetryBreaking)
    LexIncreasing(x, matrix=True)
)
