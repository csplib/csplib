---
Title:    Transshipment problem
Proposer: Özgür Akgün
Category:
- Design and configuration
- Logistics
---

This is the simple version of the transshipment problem. It is a network flow problem with three kinds of nodes: warehouses, transshipment points and customers. Warehouses act like source nodes and customers act like sink nodes. Products are to be transferred from warehouses to customers via transshipment points. In more complex real-life applications of this problem transshipment points provide a possibility for extra handling (packaging, splitting, consolidating) and/or change of transportation mode. See the [Wikipedia article](https://en.wikipedia.org/wiki/Transshipment_problem) for some variations.

Problem parameters:

- Demand per customer (unit: number of products)
- Stock per warehouse (unit: number of products)
- Cost of moving one unit of product between a warehouse and a transshipment point
- Cost of moving one unit of product between a transshipment point and a customer

Potential extensions:

- Multiple products types
- Lower/upper bounds on edge capacities

The problem can be used as a benchmark as either an optimisation problem (minimising total cost) or as a satisfaction problem (with a given upper bound for total cost).

See {prob034} for a related problem.
