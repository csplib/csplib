Title: Synchronous Optical Networking (SONET) Problem
Proposer: Peter Nightingale
Category: Network design

In the SONET problem we are given a set of nodes, and for each pair of nodes
we are given the /demand/ (which is the number of channels required to carry network traffic 
between the two nodes). The demand may be zero, in which case the two nodes do
not need to be connected. 

A SONET /ring/ connects a set of nodes. A node is installed on a ring using a piece of equipment
called an add-drop multiplexer (ADM).  Each node may be installed on more than one
ring. Network traffic can be transmitted from
one node to another only if they are both installed on the same ring. 
Each ring has an upper limit on the number of nodes, and a limit on the number
of channels. The demand of a pair of nodes may be split between multiple rings. 

The objective is to minimise the total number of ADMs used while satisfying all demands. 

The Unlimited Traffic Capacity Problem
--------

In the unlimited traffic capacity problem, the magnitude of the demands is ignored.
If a pair of nodes $n_1$ and $n_2$ has a non-zero demand, then there must exist a ring
connecting $n_1$ and $n_2$. The upper limit on the number of channels per ring 
has no significance in this simplified problem. 





