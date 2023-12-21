"""
PyCSP3 Model (see pycsp.org)


Examples:
  python CostasArray.py
  python CostasArray.py -data=10
"""

from pycsp3 import *

n = data or 8

# x[i] is the row where is put the ith mark (on the ith column)
x = VarArray(size=n, dom=range(n))

satisfy(
    # all marks are on different rows (and columns)
    AllDifferent(x),

    # all displacement vectors between the marks must be different
    [AllDifferent(x[i] - x[i + d] for i in range(n - d)) for d in range(1, n - 1)]
)

