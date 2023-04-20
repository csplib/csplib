"""
PyCSP3 Model (see pycsp.org)

Examples:
  python LowAutocorrelation.py
  python LowAutocorrelation.py -data=16
"""

from pycsp3 import *

n = data or 8

# x[i] is the ith value of the sequence to be built.
x = VarArray(size=n, dom={-1, 1})

# y[k][i] is the ith product value required to compute the kth auto-correlation
y = VarArray(size=[n - 1, n - 1], dom=lambda k, i: {-1, 1} if i < n - k - 1 else None)

# c[k] is the value of the kth auto-correlation
c = VarArray(size=n - 1, dom=lambda k: range(-n + k + 1, n - k))

satisfy(
    [y[k][i] == x[i] * x[i + k + 1] for k in range(n - 1) for i in range(n - k - 1)],

    [Sum(y[k]) == c[k] for k in range(n - 1)]
)

minimize(
    # minimizing the sum of the squares of the auto-correlation
    Sum(c[k] * c[k] for k in range(n - 1))
)


