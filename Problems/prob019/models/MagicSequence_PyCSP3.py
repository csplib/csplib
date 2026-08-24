"""
PyCSP3 Model (see pycsp.org)

Examples:
  python Magicsequence.py
  python Magicsequence.py -data=10
"""

from pycsp3 import *

n = data or 8

# x[i] is the ith value of the sequence
x = VarArray(size=n, dom=range(n))

satisfy(
    # each value i occurs exactly x[i] times in the sequence
    Cardinality(x, occurrences={i: x[i] for i in range(n)}),

    # tag(redundant-constraints)
    [Sum(x) == n, Sum((i - 1) * x[i] for i in range(n)) == 0]
)

