"""
PyCSP3 Model (see pycsp.org)

Examples:
  python DiamondFree.py
  python DiamondFree.py -data=10
"""

from pycsp3 import *

n = data or 8

# x is the adjacency matrix
x = VarArray(size=[n, n], dom=lambda i, j: {0, 1} if i != j else {0})

# y[i] is the degree of the ith node
y = VarArray(size=n, dom={i for i in range(1, n) if i % 3 == 0})

# s is the sum of all degrees
s = Var(dom={i for i in range(n, n * (n - 1) + 1) if i % 12 == 0})

satisfy(
    # ensuring the absence of diamond in the graph
    [Sum(x[i][j], x[i][k], x[i][l], x[j][k], x[j][l], x[k][l]) <= 4 for i, j, k, l in combinations(n, 4)],

    # ensuring that the graph is undirected (symmetric)
    [x[i][j] == x[j][i] for i, j in combinations(n, 2)],

    # computing node degrees
    [Sum(x[i]) == y[i] for i in range(n)],

    # computing the sum of node degrees
    Sum(y) == s,

    # tag(symmetry-breaking)
    [Decreasing(y), LexIncreasing(x)]
)
