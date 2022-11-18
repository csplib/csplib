"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python Clique.py -data=Cl10.json
"""

from pycsp3 import *

matrix = data
n = len(matrix)

# x[i] is 1 iff the ith node belongs to the clique
x = VarArray(size=n, dom={0, 1})

satisfy(
    # forbidding the presence of any two nodes that are not bidirectionally linked
    (x[i], x[j]) not in {(1, 1)} for i, j in combinations(range(n), 2) if matrix[i][j] != 1 or matrix[j][i] != 1
)

maximize(
    # maximizing the size of the clique
    Sum(x)
)
