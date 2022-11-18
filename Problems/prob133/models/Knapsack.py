"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python Knapsack.py -data=Knapsack_20-50-00.json
"""

from pycsp3 import *

capacity, items = data
weights, values = zip(*items)
nItems = len(items)

# x[i] is 1 iff the ith item is selected
x = VarArray(size=nItems, dom={0, 1})

satisfy(
    # not exceeding the capacity of the knapsack
    x * weights <= capacity
)

maximize(
    # maximizing summed up value (benefit)
    x * values
)
