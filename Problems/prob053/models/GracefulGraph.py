"""
PyCSP3 Model (see pycsp.org)

Examples:
  python GracefulGraph.py
  python GracefulGraph.py -data=[3,5]
"""

from pycsp3 import *

k, p = data or (2, 4)  # k is the size of each clique K (number of nodes) -- p is the size of each path P (or equivalently, number of cliques)
nEdges = int(((k * (k - 1)) * p) / 2 + k * (p - 1))

# cn[i][j] is the color of the jth node of the ith clique
cn = VarArray(size=[p, k], dom=range(nEdges + 1))

# ce[i][j1][j2] is the color of the edge (j1,j2) of the ith clique, for j1 strictly less than j2
ce = VarArray(size=[p, k, k], dom=lambda i, j1, j2: range(1, nEdges + 1) if j1 < j2 else None)

# cp[i][j] is the color of the jth edge of the ith path
cp = VarArray(size=[p - 1, k], dom=range(1, nEdges + 1))

satisfy(
    # all nodes are colored differently
    AllDifferent(cn),

    # all edges are colored differently
    AllDifferent(ce + cp),

    # computing colors of edges from colors of nodes
    [
        [ce[i][j1][j2] == abs(cn[i][j1] - cn[i][j2]) for i in range(p) for j1, j2 in combinations(k, 2)],

        [cp[i][j] == abs(cn[i][j] - cn[i + 1][j]) for i in range(p - 1) for j in range(k)]
    ]
)
