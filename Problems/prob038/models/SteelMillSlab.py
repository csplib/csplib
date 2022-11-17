"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Exampless:
  python SteelMillSlab.py -data=SteelMillSlab_bench_2_0.json
  python SteelMillSlab.py -data=SteelMillSlab_bench_2_0.json -variant=01
"""

from pycsp3 import *

capacities, orders = data
possibleLosses = [min(v for v in [0] + capacities if v >= i) - i for i in range(max(capacities) + 1)]
sizes, colors = zip(*orders)
allColors = sorted(set(colors))
colorGroups = [[i for i, order in enumerate(orders) if order.color == color] for color in allColors]
nOrders, nSlabs, nColors = len(orders), len(orders), len(allColors)

# sb[i] is the slab used to produce the ith order
sb = VarArray(size=nOrders, dom=range(nSlabs))

# ld[j] is the load of the jth slab
ld = VarArray(size=nSlabs, dom=range(max(capacities) + 1))

# ls[j] is the loss of the jth slab
ls = VarArray(size=nSlabs, dom=set(possibleLosses))

if not variant():
    satisfy(
        # computing (and checking) the load of each slab
        [[sb[i] == j for i in range(nOrders)] * sizes == ld[j] for j in range(nSlabs)],

        # computing the loss of each slab
        [(ld[j], ls[j]) in {(i, loss) for i, loss in enumerate(possibleLosses)} for j in range(nSlabs)],

        # no more than two colors for each slab
        [Sum(disjunction(sb[i] == j for i in g) for g in colorGroups) <= 2 for j in range(nSlabs)]
    )

elif variant("01"):
    # y[j][i] is 1 iff the jth slab is used to produce the ith order
    y = VarArray(size=[nSlabs, nOrders], dom={0, 1})

    # z[j][c] is 1 iff the jth slab is used to produce an order of color c
    z = VarArray(size=[nSlabs, nColors], dom={0, 1})

    satisfy(
        # linking variables sb and y
        [iff(sb[i] == j, y[j][i]) for j in range(nSlabs) for i in range(nOrders)],

        # linking variables sb and z
        [imply(sb[i] == j, z[j][allColors.index(orders[i].color)]) for j in range(nSlabs) for i in range(nOrders)],

        # computing (and checking) the load of each slab
        [y[j] * sizes == ld[j] for j in range(nSlabs)],

        # computing the loss of each slab
        [(ld[j], ls[j]) in {(i, loss) for i, loss in enumerate(possibleLosses)} for j in range(nSlabs)],

        # no more than two colors for each slab
        [Sum(z[j]) <= 2 for j in range(nSlabs)]
    )

satisfy(
    # tag(redundant-constraints)
    Sum(ld) == sum(sizes),

    # tag(symmetry-breaking)
    [
        Decreasing(ld),

        [sb[i] <= sb[j] for i, j in combinations(range(nOrders), 2) if orders[i] == orders[j]]
    ]
)

minimize(
    # minimizing summed up loss
    Sum(ls)
)

