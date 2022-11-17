"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python CarSequencing.py -data=dingbas.json
"""

from pycsp3 import *
from math import ceil

classes, limits = data
demands = [demand for demand, _ in classes]
nCars, nClasses, nOptions = sum(demands), len(classes), len(limits)


# c[i] is the class of the ith assembled car
c = VarArray(size=nCars, dom=range(nClasses))

# o[i][k] is 1 if the ith assembled car has option k
o = VarArray(size=[nCars, nOptions], dom={0, 1})

satisfy(
    # building the right numbers of cars per class
    Cardinality(c, occurrences={j: demands[j] for j in range(nClasses)}),

    # computing assembled car options
    (c[i], *o[i]) in {(j, *options) for j, (_, options) in enumerate(classes)} for i in range(nCars),

    # respecting option frequencies
    [Sum(o[i:i + den, k]) <= num for k, (num, den) in enumerate(limits) for i in range(nCars) if i <= nCars - den]
)
