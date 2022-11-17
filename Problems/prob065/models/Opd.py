"""
PyCSP3 Model (see pycsp.org)

Examples:
  python Opd.py
  python Opd.py -data=[4,6,4]
  python Opd.py -data=[4,6,4] -variant=aux
"""

from pycsp3 import *

v, b, r = data or (4, 4, 4)

# x[i][j] is the value at row i and column j
x = VarArray(size=[v, b], dom={0, 1})

satisfy(
    # each row sums to 'r'
    Sum(x[i]) == r for i in range(v)
)

if not variant():
    minimize(
        # minimizing the maximum value of dot products between all pairs of distinct rows
        Maximum(x[i] * x[j] for i, j in combinations(range(v), 2))
    )

elif variant("aux"):
    # s[i][j][k] is the scalar variable for the product of x[i][k] and x[j][k]
    s = VarArray(size=[v, v, b], dom=lambda i, j, k: {0, 1} if i < j else None)

    satisfy(
        # computing scalar variables
        s[i][j][k] == x[i][k] * x[j][k] for i, j in combinations(range(v), 2) for k in range(b)
    )

    minimize(
        # minimizing the maximum value of dot products between all pairs of distinct rows
        Maximum(Sum(s[i][j]) for i, j in combinations(range(v), 2))
    )

satisfy(
    # tag(symmetry-breaking)
    LexIncreasing(x, matrix=True)
)
