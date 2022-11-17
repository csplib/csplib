"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python NursingWorkload.py -data=zones.json
"""

from pycsp3 import *

nNurses, minPatientsPerNurse, maxPatientsPerNurse, maxWorkloadPerNurse, demands = data
patients = [(i, demand) for i, t in enumerate(demands) for demand in t]
nPatients, nZones = len(patients), len(demands)

lb = sum(sorted([demand for i, t in enumerate(demands) for demand in t])[:minPatientsPerNurse])

# p[i] is the nurse assigned to the ith patient
p = VarArray(size=nPatients, dom=range(nNurses))

# w[k] is the workload of the kth nurse
w = VarArray(size=nNurses, dom=range(lb, maxWorkloadPerNurse + 1))

satisfy(
    Cardinality(p, occurrences={k: range(minPatientsPerNurse, maxPatientsPerNurse + 1) for k in range(nNurses)}),

    [p[i] != p[j] for i, j in combinations(range(nPatients), 2) if patients[i][0] != patients[j][0]],

    [w[k] == Sum(c * (p[i] == k) for i, (_, c) in enumerate(patients)) for k in range(nNurses)],

    # tag(symmetry-breaking)
    [p[z] == z for z in range(nZones)],

    Increasing(w)
)

minimize(
    Sum(w[k] * w[k] for k in range(nNurses))
)
