---
Title: Discrete Lot Sizing Problem
Proposer: 
    - Vinasetan Ratheil Houndji
    - Pierre Schaus 
    - Laurence Wolsey
    - Yves Deville
Category: Scheduling and related problems
---

Discrete Lot Sizing and Scheduling Problem (DLSP) is a production planning problem which consists of determining a minimal cost production schedule (production costs, setup
costs, changeover costs, stocking costs, etc.), such that machine capacity restrictions are not violated, and demand for all products is satisfied. The planning horizon is
discrete and finite. 

The variant described here is the one used for experiments in [The StockingCost Constraint](data/stockingCostConstraint.pdf).

It is a multi-item, single machine problem with capacity of production limited to one per period. 
There are storage costs and sequence-dependent changeover costs, respecting the triangle inequality. 
Each order consisting of one unit of a particular item has a due date and must be produced at latest by its due date. 
The stocking (inventory) cost of an order is proportional to the number of periods between the due date and the production period. 
The changeover cost $q^{i,j}$ is induced when passing from the production of item $i$ to another one $j$ with $q^{i,i} = 0, \forall i$. Here, backlogging is not allowed. 
The objective is to assign a production period for each order respecting its due date and the machine capacity constraint so as to minimize the sum of stocking costs and
changeover costs.

Example : 
Consider the problem with the following input data: number of items type $nbItems = 2$; number of periods $nbPeriods = 5$; stocking cost $h= 2$; demand times for items of type
1 $d^1_{t \in \{1,\ldots,5\}} = (0, 1, 0, 0, 1)$ and for items of type 2 $d^2_{t \in \{1,\ldots,5\}} = (1, 0, 0, 0, 1)$; $q^{1,2} = 5$, $q^{2,1} = 3$. 
A feasible solution of this problem is $productionPlan = (2, 1, 2, 0, 1)$ which means that item $2$ will be produced in period $1$; 
item $1$ in period $2$; item $2$ in period $3$ and item $1$ in period $5$. 
Note that there is no production in period $4$, it is an idle period. 
The cost associated to this solution is $q^{2,1}+q^{1,2}+q^{2,1}+2*h = 15$ but it is not the optimal cost. 
The optimal solution is $productionPlan = (2,1,0,1,2)$ with the cost $q^{2,1}+q^{1,2}+h= 10$.

---

A [Simulated Annealing metaheuristic approach](https://doi.org/10.1016/j.cie.2017.10.017) along with a dataset of [large-size instances](data/UniUD-LotSizingLargeInstances.zip) has been developed by Sara Ceschia, Luca Di Gaspero and Andrea Schaerf.