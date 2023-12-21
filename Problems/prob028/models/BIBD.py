"""
PyCSP3 Model (see pycsp.org)

Examples:
  python BIBD.py
  python BIBD.py -data=[9,0,0,3,9]
  python BIBD.py -data=[9,0,0,3,9] -variant=aux
"""

from pycsp3 import *

v, b, r, k, l = data or (6, 0, 0, 3, 8)
b = (l * v * (v - 1)) // (k * (k - 1)) if b == 0 else b  # when specified at 0, b is automatically computed
r = (l * (v - 1)) // (k - 1) if r == 0 else r  # when specified at 0, r is automatically computed

# x[i][j] is the value of the matrix at row i and column j
x = VarArray(size=[v, b], dom={0, 1})

if not variant():
    satisfy(
        # constraints on rows
        [Sum(row) == r for row in x],

        # constraints on columns
        [Sum(col) == k for col in columns(x)],

        # scalar constraints with respect to lambda
        [row1 * row2 == l for row1, row2 in combinations(x, 2)]
    )

elif variant("aux"):
    # s[i][j][k] is the product of x[i][k] and x[j][k]
    s = VarArray(size=[v, v, b], dom={0, 1})

    satisfy(
        # constraints on rows
        [Sum(x[i]) == r for i in range(v)],

        # constraints on columns
        [Sum(x[:, j]) == k for j in range(b)],

        # computing scalar variables
        [s[i][j][k] == x[i][k] * x[j][k] for i, j in combinations(v, 2) for k in range(b)],

        # scalar constraints with respect to lambda
        [Sum(s[i][j]) == l for i, j in combinations(v, 2)]
    )

satisfy(
    # Increasingly ordering both rows and columns  tag(symmetry-breaking)
    LexIncreasing(x, matrix=True)
)
