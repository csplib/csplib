"""
PyCSP3 Model (see pycsp.org)

Examples:
  python3 GolombRuler.py
  python3 GolombRuler.py -data=10
  python3 GolombRuler.py -data=10 -variant=dec
  python3 GolombRuler.py -data=10 -variant=aux
"""

from pycsp3 import *

n = data or 8
ub = n * n + 1  # a trivial upper-bound of an optimal ruler length

# x[i] is the position of the ith tick
x = VarArray(size=n, dom=range(ub))

if not variant():
    satisfy(
        # all distances are different
        AllDifferent(abs(x[i] - x[j]) for i, j in combinations(n, 2))
    )
elif variant("dec"):
    satisfy(
        # all distances are different
        abs(x[i] - x[j]) != abs(x[k] - x[l]) for i, j in combinations(range(n), 2) for k, l in combinations(range(i + 1, n), 2)
    )
elif variant("aux"):
    # y[i][j] is the distance between x[i] and x[j], for i strictly less than j
    y = VarArray(size=[n, n], dom=lambda i, j: range(1, ub) if i < j else None)

    satisfy(
        # all distances are different
        AllDifferent(y),

        # linking variables from both arrays
        [x[j] == x[i] + y[i][j] for i, j in combinations(n, 2)]
    )
    annotate(decision=x)

satisfy(
    # tag(symmetry-breaking)
    [x[0] == 0, Increasing(x, strict=True)]
)

minimize(
    # minimizing the position of the rightmost tick
    Maximum(x)
)


