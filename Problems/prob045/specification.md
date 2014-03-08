Title:    The Covering Array Problem
Proposer: Evgeny Selensky
Category: Combinatorial mathematics


The covering array problem is formulated as follows.

A covering array $CA(t,k,g)$ of size $b$ and strength $t$, is a $k x b$ array $A = (a_{ij})$ over $Z_g = {0,1,2,...,g-1}$ with the property that for any t distinct rows $1 <= r_1 <= r_2 <= ... <= r_t <= k$, and any member $(x_1, x_2, ..., x_t)$ of $Z^{t_g}$ there exists at least one column $c$ such that $x_i$ equals the $(r_i,c)-th$ element of $A$ for all $1 <= i <= t$.

A covering array number $CAN(t,k,g)$ is the smallest b such that there exists a $CA(t,k,g)$ of size $b$.

Informally, any t distinct rows of the covering array must encode column-wise all numbers from $0$ to $g^{t-1}$ (repititions are allowed).

An example of covering array $CA(3,5,2)$ over the Boolean alphabet ${0,1}$ is:

    0	0	0	0	0	1	1	1	1	1
    0	0	0	1	1	0	0	1	1	1
    0	0	1	0	1	0	1	0	1	1
    0	1	0	0	1	0	1	1	0	1
    0	1	1	1	0	1	0	0	0	1
    
In this array, any $t = 3$ rows encode all numbers from $0$ (when the respective elements are ${0,0,0}$) to $2^3 - 1 = 7$ (when the elements are ${1,1,1}$). E.g., the three top rows encode the following numbers (from left to right): $0,0,1,2,3,4,5,6,7,7$; the three bottom rows encode numbers: $0,3,5,1,6,1,6,2,4,7$.

It has been proved in [3] that $CAN(3,5,2) = 10$ and therefore the presented array is an optimal solution to $CA(3,5,2)$.

The problem comes from hardware and software testing.