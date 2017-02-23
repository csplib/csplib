---
Title:    Product Matrix Travelling Salesman Problem
Proposer:
    - Sascha Van Cauwelaert
    - Pierre Schaus
Category: Combinatorial mathematics
---

Given two vectors of $n$ elements $C$ and $P$, one can construct a simple graph $G$ with $n$ vertices, such that the directed edge from the vertex $i$ to the vertex $j \neq i$ has a cost equal to $C(i) \cdot P(j)$. The distance matrix is therefore the matrix product between the vectors $C$ and $P$, hence the name of the problem. The problem consists in finding an Hamiltonian circuit of minimum total cost in the graph $G$. This problem was first described in cite{plante1987product} and shown to be NP-hard in cite{sarvanov1980complexity,gilmore1985well}.
