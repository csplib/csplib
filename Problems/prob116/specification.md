---
Title:    Vellino's Problem
Proposer: Özgür Akgün
---

(See cite{van1999constraint})

Given a supply of components and bins of various types, Vellino's problem consists of assigning the components to the bins so that the bin constraints are satisfied and the smallest possible number of bins is used. There are five types of components, i.e., glass, plastic, steel, wood, and copper, and three types of bins, i.e., red, blue, green. The bins must obey a variety of configuration constraints. Containment constraints specify which components can go into which bins: red bins cannot contain plastic of steel, blue bins cannot contain wood or plastic, and green bins cannot contain steel or glass. Other constraints specify a limit for certain component types for some bins: red bins contain at most one wooden component and green bins contain at most two wooden components.
Requirement constraints specify some compatibility constraints between the components: wood requires plastic, glass excludes copper and copper excludes plastic. In addition, we are given an initial capacity for each bin, i.e., red bins have a capacity of 3 components, blue bins of 1 and green bins of 4 and a demand for each component, i.e., 1 glass, 2 plastic, 1 steel, 3 wood, and 2 copper components.
Finally, demands of the components must be met and the bin capacities should not be exceeded.
