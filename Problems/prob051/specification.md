---
Title:    Tank Allocation
Proposer: Pierre Schaus
Category: 
	- Bin packing
	- Partitioning and related problems
---
          


The tank allocation problem involves the assignment of different cargoes (volumes of chemical products to be shipped by the vessel) to the available tanks of the vessel. The loading plans of bulk vessels are generally generated manually by the vessel planners although it is difficult to generate high quality solutions. The constraints to satisfy are mainly segregation constraints:

1.  Prevent chemicals from being loaded into certain types of tanks because:
    -   The chemical may need to have its temperature managed and the tank needs to be equipped with a heating system.
    -   The tank must be resistant to the chemical.
    -   A tank may still be contaminated by previous cargoes incompatible with the chemical.

2.  Prevent some pairs of cargoes to be placed next to each other: not only the chemical interactions between the different cargoes need to be considered but also the temperature at which they need to be transported. Too different temperature requirements for adjacent tanks cause the second one to solidify due to cooling off by the first cargo or the first may become chemically unstable due to heating up of the second cargo.

In order to minimise the costs and inconvenience of tank cleaning, an ideal loading plan should maximise the total volume of unused tanks (i.e. free space).
