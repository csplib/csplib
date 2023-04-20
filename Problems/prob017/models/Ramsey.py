"""
PyCSP3 Model (see pycsp.org)

Examples:
  python3 Ramsey.py
  python3 Ramsey.py -data=10
"""

from pycsp3 import *

n = data or 8

# x[i][j] is the color of the edge between nodes i and j
x = VarArray(size=[n, n], dom=lambda i, j: range((n * (n - 1)) // 2) if i < j else None)

satisfy(
    # no monochromatic triangle in the graph
    NValues(x[i][j], x[i][k], x[j][k]) > 1 for (i, j, k) in combinations(range(n), 3)
)

minimize(
    Maximum(x)
)
