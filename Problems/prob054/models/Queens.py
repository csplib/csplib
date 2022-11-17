"""
PyCSP3 Model (see pycsp.org)

Examples:
  python Queens.py
  python Queens.py -data=6
  python Queens.py -data=6 -variant=v1
  python Queens.py -data=6 -variant=v2
"""

from pycsp3 import *

n = data or 8

# q[i] is the column where is put the ith queen (at row i)
q = VarArray(size=n, dom=range(n))

if not variant():
    satisfy(
        AllDifferent(q),

        # controlling no two queens on the same upward diagonal
        AllDifferent(q[i] + i for i in range(n)),

        # controlling no two queens on the same downward diagonal
        AllDifferent(q[i] - i for i in range(n))
    )
elif variant("v1"):
    satisfy(
        AllDifferent(q),

        [abs(q[i] - q[j]) != j - i for i, j in combinations(n, 2)]
    )

elif variant("v2"):
    satisfy(
        (q[i] != q[j]) & (abs(q[i] - q[j]) != j - i) for i, j in combinations(n, 2)
    )
