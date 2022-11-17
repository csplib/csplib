"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python Sonet.py -data=Sonet_sonet1.json
"""

from pycsp3 import *

n, m, r, connections = data

# x[i][j] is 1 if the ith ring contains the jth node
x = VarArray(size=[m, n], dom={0, 1})

table = {tuple(1 if j // 2 == i else ANY for j in range(2 * m)) for i in range(m)}

satisfy(
    [(x[i][j1] if k == 0 else x[i][j2] for i in range(m) for k in range(2)) in table for (j1, j2) in connections],

    # respecting the capacity of rings
    [Sum(x[i]) <= r for i in range(m)],

    # tag(symmetry-breaking)
    LexIncreasing(x)
)

minimize(
    # minimizing the number of nodes installed on rings
    Sum(x)
)
