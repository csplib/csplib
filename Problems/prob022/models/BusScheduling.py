"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python BusScheduling.py -data=BusScheduling_r1.json
"""

from pycsp3 import *

nTasks, shifts = data
nShifts = len(shifts)

# x[i] is 1 iff the ith shift is selected
x = VarArray(size=nShifts, dom={0, 1})

satisfy(
    # each task is covered by exactly one shift
    Count(x[i] for i, shift in enumerate(shifts) if t in shift) == 1 for t in range(nTasks)
)

minimize(
    # minimizing the number of shifts
    Sum(x)
)
