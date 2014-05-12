The model in [1] introduces two families of variables:

1.  One variable $X[t]$ for each tank t representing the product type to be placed inside that tank. The initial domain is $0…P$ (some tanks might be impossible for some products), 0 represents a dummy product meaning that no product is assigned to the tank $t$ (in this case $X[t]=0$).
2.  One load variable L[p] for each of the product $0..P$ to be loaded on the vessel. This variable represents how much tank volume is available/assigned to each product. The initial lower bound on this variable$ L[p]$ is the quantity of product p to be loaded. This guarantees enough space is available to accommodate this product p $(L[0] \>= 0)$.

These two sets of variables are linked with a bin-packing constraint$(X,caps,L)$ where $c$ is the vector of capacities for each tank. This constraint enforces $L[p]$ = $sum\_{t in tanks}$ ($X[t]=p)\*capa[t] \forall p$

Some table constraints can be added on some pairs of neighbouring tank variables $x[i],x[j]$ to avoid incompatible products to be located next to each other.

The objective is to maximise the empty volume i.e. variable $L[0]$.

[1] Pierre Schaus, Jean-Charles Régin, Rowan Van Schaeren, Wout Dullaert, Birger Raa: Cardinality Reasoning for Bin-Packing Constraint: Application to a Tank Allocation Problem. CP 2012: 815-822
