"""
PyCSP3 Model (see pycsp.org)

Examples:
  python Steiner3.py
  python Steiner3.py -data=6
"""

from pycsp3 import *

n = data or 6
nTriples = (n * (n - 1)) // 6

table = {(i1, i2, i3, j1, j2, j3) for (i1, i2, i3, j1, j2, j3) in product(range(1, n + 1), repeat=6) if
         different_values(i1, i2, i3) and different_values(j1, j2, j3) and len({i for i in {i1, i2, i3} if i in {j1, j2, j3}}) <= 1}

# x[i] is the ith triple of values
x = VarArray(size=[nTriples, 3], dom=range(1, n + 1))

satisfy(
    # each triple must be formed of strictly increasing integers
    [Increasing(triple, strict=True) for triple in x],

    # each pair of triples must share at most one value
    [(triple1 + triple2) in table for (triple1, triple2) in combinations(x, 2)]
)
