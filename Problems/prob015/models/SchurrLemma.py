"""
PyCSP3 Model (see pycsp.org)

The variant 'mod' corresponds to the one proposed in [Bessiere Meseguer Freuder Larrosa, On forward checking for non-binary constraint satisfaction, 2002].

Examples:
  python SchurrLemma.py
  python SchurrLemma.py -data=[10,10]
  python SchurrLemma.py -data=[10,10] -variant=mod
"""

from pycsp3 import *

n, d = data or (8, 8)  # n is the number of balls -- d is the number of boxes

# x[i] is the box where the ith ball is put
x = VarArray(size=n, dom=range(d))

if not variant():
    satisfy(
        NValues(x[i], x[j], x[k]) > 1 for (i, j, k) in product(range(n), repeat=3) if i < j and i + 1 + j == k
    )
elif variant("mod"):
    satisfy(
        AllDifferent(x[i], x[j], x[k]) for (i, j, k) in product(range(n), repeat=3) if i < j and i + 1 + j == k
    )
