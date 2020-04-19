---
Title:    Warehouse Location Problem
Proposer: Brahim Hnich
Category:
- Design and configuration
- Logistics
---

In the <i>Warehouse Location</i> problem (WLP), a company considers opening warehouses at some candidate locations in order to supply its existing stores.
Each possible warehouse has the same maintenance cost, and a capacity designating the maximum number of stores that it can supply.
Each store must be supplied by exactly one open warehouse.

The supply cost to a store depends on the warehouse.
The objective is to determine which warehouses to open, and which of these warehouses should supply the various stores, such that the sum of the maintenance and supply costs is minimized.

As an example (from the OPL book), consider the following data:

```
fixed = 30;
Warehouses = { Bonn, Bordeaux, London, Paris, Rome };
nbStores = 10; //labeled from 0 to 9
capacity = [1,4,2,1,3]; // capacity is indexed by Warehouses

// supplyCost in indexed by Stores(0..9) and the set of Warehouses

supplyCost = [ [ 20, 24, 11, 25, 30 ]
             , [ 28, 27, 82, 83, 74 ]
             , [ 74, 97, 71, 96, 70 ]
             , [ 2, 55, 73, 69, 61 ]
             , [ 46, 96, 59, 83, 4 ]
             , [ 42, 22, 29, 67, 59 ]
             , [ 1, 5, 73, 59, 56 ]
             , [ 10, 73, 13, 43, 96 ]
             , [ 93, 35, 63, 85, 46 ]
             , [ 47, 65, 55, 71, 95 ]
             ];

```

Then, an optimal solution has value `383`, where:

Stores of Bonn = `{3}`

Stores of Bordeaux = `{8,6,5,1}`

Stores of London = `{9,7}`

Stores of Paris = `{}`

Stores of Rome = `{4,2,0}`

