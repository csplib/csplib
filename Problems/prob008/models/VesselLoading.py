"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python VesselLoading.py -data=VesselLoading-inst2.json
"""

from pycsp3 import *

deckWidth, deckHeight, containers, separations = data
nContainers = len(containers)


def sep_pairs():
    groups = [[i for i, container in enumerate(containers) if container.type == k] for k in range(max(container.type for container in containers) + 1)]
    return [(i, j, distance) for (type1, type2, distance) in separations for i in groups[type1] for j in groups[type2]]


# x[i] is the x-coordinate of the ith container
x = VarArray(size=nContainers, dom=range(deckWidth))

# y[i] is the y-coordinate of the ith container
y = VarArray(size=nContainers, dom=range(deckHeight))

# w[i] is the width of the ith container
w = VarArray(size=nContainers, dom=lambda i: {containers[i].width, containers[i].height})

# h[i] is the height of the ith container
h = VarArray(size=nContainers, dom=lambda i: {containers[i].width, containers[i].height})

# r[i] is 1 iff the ith container is rotated by 90 degrees
r = VarArray(size=nContainers, dom={0, 1})

satisfy(
    # horizontal control
    [x[i] + w[i] <= deckWidth for i in range(nContainers)],

    # vertical control
    [y[i] + h[i] <= deckHeight for i in range(nContainers)],

    # managing rotation
    [(r[i], w[i], h[i]) in {(0, width, height), (1, height, width)} for i, (width, height, _) in enumerate(containers)],

    # no overlapping between containers
    NoOverlap(origins=[(x[i], y[i]) for i in range(nContainers)], lengths=[(w[i], h[i]) for i in range(nContainers)]),

    # respecting separations between containers according to their types
    [(x[i] + w[i] + sep <= x[j]) | (x[j] + w[j] + sep <= x[i]) | (y[i] + h[i] + sep <= y[j]) | (y[j] + h[j] + sep <= y[i]) for (i, j, sep) in sep_pairs()]
)
