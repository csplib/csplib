"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python ProgressiveParty.py -data=ProgressiveParty_example.json
"""

from pycsp3 import *

nPeriods, boats = data
nBoats = len(boats)
capacities, crews = zip(*boats)


def minimal_number_of_hosts():
    nPersons = sum(crews)
    cnt, acc = 0, 0
    for capacity in sorted(capacities, reverse=True):
        if acc >= nPersons:
            return cnt
        acc += capacity
        cnt += 1


# h[b] indicates if the boat b is a host boat
h = VarArray(size=nBoats, dom={0, 1})

# s[b][p] is the scheduled (visited) boat by the crew of boat b at period p
s = VarArray(size=[nBoats, nPeriods], dom=range(nBoats))

# g[b1][p][b2] is 1 if s[b1][p] = b2
g = VarArray(size=[nBoats, nPeriods, nBoats], dom={0, 1})

satisfy(
    # identifying host boats
    [iff(s[b][p] == b, h[b]) for b in range(nBoats) for p in range(nPeriods)],

    # identifying host boats (from visitors)
    [h[s[b][p]] == 1 for b in range(nBoats) for p in range(nPeriods)],

    # channeling variables from arrays s and g
    [Channel(g[b][p], s[b][p]) for b in range(nBoats) for p in range(nPeriods)],

    # boat capacities must be respected
    [g[:, p, b] * crews <= capacities[b] for b in range(nBoats) for p in range(nPeriods)],

    # a guest boat cannot revisit a host
    [AllDifferent(s[b], excepting=b) for b in range(nBoats)],

    # guest crews cannot meet more than once
    [Sum(s[b1][p] == s[b2][p] for p in range(nPeriods)) <= 1 for b1, b2 in combinations(range(nBoats), 2)],

    # ensuring a minimum number of hosts  tag(redundant-constraint)
    Sum(h) >= minimal_number_of_hosts()
)

minimize(
    # minimizing the number of host boats
    Sum(h)
)
