"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python CVRP.py -data=example.json
"""

from pycsp3 import *

nNodes, capacity, demands, distances = data
nVehicles = nNodes // 4  # This is a kind of hard coding, which can be at least used for Set A (Augerat, 1995)


def max_tour():
    t = sorted(demands)
    i, s = 1, 0
    while i < nNodes and s < capacity:
        s += t[i]
        i += 1
    return i - 2


nSteps = max_tour()

# c[i][j] is the jth customer (step) during the tour of the ith vehicle
c = VarArray(size=[nVehicles, nSteps], dom=range(nNodes))

# d[i][j] is the demand of the jth customer during the tour of the ith vehicle
d = VarArray(size=[nVehicles, nSteps], dom=demands)

satisfy(
    AllDifferent(c, excepting=0),

    # ensuring that all demands are satisfied
    Cardinality(c, occurrences={0: nVehicles * nSteps - nNodes + 1} + {i: 1 for i in range(1, nNodes)}),

    # no holes permitted during tours
    [(c[i][j] != 0) | (c[i][j + 1] == 0) for i in range(nVehicles) for j in range(nSteps - 1)],

    # computing the collected demands
    [demands[c[i][j]] == d[i][j] for i in range(nVehicles) for j in range(nSteps)],

    # not exceeding the capacity of each vehicle
    [Sum(d[i]) <= capacity for i in range(nVehicles)],

    # tag(symmetry-breaking)
    Decreasing(c[:, 0])
)

minimize(
    # minimizing the total traveled distance by vehicles
    Sum(distances[0][c[i][0]] for i in range(nVehicles))
    + Sum(distances[c[i][j]][c[i][j + 1]] for i in range(nVehicles) for j in range(nSteps - 1))
    + Sum(distances[c[i][-1]][0] for i in range(nVehicles))
)

