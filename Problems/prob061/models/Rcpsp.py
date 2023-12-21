"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python Rcpsp.py -data=Rcpsp_j30-01-01.json
"""

from pycsp3 import *

horizon, capacities, jobs = data
nJobs = len(jobs)

# s[i] is the starting time of the ith job
s = VarArray(size=nJobs, dom=lambda i: {0} if i == 0 else range(horizon))

satisfy(
    # precedence constraints
    [s[i] + duration <= s[j] for i, (duration, successors, _) in enumerate(jobs) for j in successors],

    # resource constraints
    [Cumulative(tasks=[(s[i], duration, quantities[k]) for i, (duration, _, quantities) in enumerate(jobs) if quantities[k] > 0]) <= capacity
     for k, capacity in enumerate(capacities)]
)

minimize(
    s[-1]
)

