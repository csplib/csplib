"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python BlockedQueens.py -data=example.json
"""

from pycsp3 import *

n, blocks = data

# q[i] is the column where is put the ith queen (at row i)
q = VarArray(size=n, dom=range(n))

satisfy(
    # respecting blocks
    [q[i] != j for (i, j) in blocks],

    # no two queens on the same column
    AllDifferent(q),

    # no two queens on the same diagonal
    [abs(q[i] - q[j]) != abs(i - j) for i, j in combinations(n, 2)]
)

