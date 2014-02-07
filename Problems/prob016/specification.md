Title:    Traffic Lights
Proposer: Toby Walsh
          Walter Hower 
Category:


Consider a four way traffic junction with eight traffic lights. Four of the traffic lights are for the vehicles and can be represented by the variables V1 to V4 with domains {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
The constraints on these variables can be modelled by quaternary constraints on (Vi, Pi, Vj, Pj ) for $1\leq i\leq 4, j=(1+i)\ mod\ 4$ which allow just the tuples {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.

We are interested in the set of all globally consistent 8-tuples (which reflects the evolution of the traffic light sequence).
