"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python WarehouseLocation.py -data=Warehouse_example.json
  python WarehouseLocation.py -data=Warehouse_example.txt -dataparser=Warehouse_Parser.py
  python WarehouseLocation.py -data=Warehouse_example.txt -dataparser=Warehouse_Parser.py -variant=compact
"""

from pycsp3 import *

cost, capacities, costs = data  # cost is the fixed cost when opening a warehouse
nWarehouses, nStores = len(capacities), len(costs)

# w[i] is the warehouse supplying the ith store
w = VarArray(size=nStores, dom=range(nWarehouses))

satisfy(
    # capacities of warehouses must not be exceeded
    Count(w, value=j) <= capacities[j] for j in range(nWarehouses)
)

if not variant():
    # c[i] is the cost of supplying the ith store
    c = VarArray(size=nStores, dom=lambda i: costs[i])

    # o[j] is 1 if the jth warehouse is open
    o = VarArray(size=nWarehouses, dom={0, 1})

    satisfy(
        # the warehouse supplier of the ith store must be open
        [o[w[i]] == 1 for i in range(nStores)],

        # computing the cost of supplying the ith store
        [costs[i][w[i]] == c[i] for i in range(nStores)]
    )

    minimize(
        # minimizing the overall cost
        Sum(c) + Sum(o) * cost
    )

elif variant("compact"):
    minimize(
        # minimizing the overall cost
        Sum(costs[i][w[i]] for i in range(nStores)) + NValues(w) * cost
    )

""" Comments
1) when compiling the 'compact' variant, some auxiliary variables are automatically introduced
   in order to remain in the perimeter of XCSP3-core   
2) it is possible to replace the first group of constraints Count by:
      Cardinality(w, occurrences={j: range(capacities[j]+1) for j in range(nWarehouses)})
    or
      BinPacking(w, sizes=1, limits=capacities)
"""
