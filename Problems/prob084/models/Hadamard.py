"""
PyCSP3 Model (see pycsp.org)

Examples:
  python Hadamard.py           // using default data
  python Hademard.py -data=21
"""

from pycsp3 import *

n = data or 35
assert n % 2 == 1
m = (n - 1) // 2

# x[i] is the ith value of the first sequence
x = VarArray(size=n, dom={-1, 1})

# y[i] is the ith value of the second sequence
y = VarArray(size=n, dom={-1, 1})

satisfy(
    Sum(x) == 1,

    Sum(y) == 1,

    # quadratic constraints
    [Sum(x[i] * x[(i + k) % n] for i in range(n)) + Sum(y[i] * y[(i + k) % n] for i in range(n)) == -2 for k in range(1, m + 1)]
)
