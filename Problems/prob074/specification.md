---
Title:    Maximum Clique
Proposer: Ciaran McCreesh
Category: Combinatorial mathematics
---

Given a simple undirected graph $G = (V,E)$, where $V$ is the set of vertices
and $E$ the set of undirected edges, a clique is a subset of $V$ such that each
distinct pair of vertices in this subset are adjacent. The maximum clique
problem is to find a clique of largest cardinality within a given graph. (The
related clique enumeration problem is to enumerate all maximal cliques---that
is, cliques which cannot be extended by adding an additional vertex.)

The second DIMACS implementation challenge studied this problem, and provided a
standard set of benchmark instances in a simple file format. These instances
are of varying size and difficulty: some should be trivial, but a few are still
open. A simple example of a file is:

<pre>
c Lines that start with a c are comments. There is one line at the start
c which starts with either "p edge" or "p col", followed by the number of
c vertices and the (possibly incorrect) number of edges. The e lines each
c describe an edge. Some files have blank lines.
p edge 5 6
e 1 2
e 2 3
e 3 4
e 4 1
e 3 5
e 4 5
</pre>

This describes a graph with 5 vertices (which are numbered 1 to 5) and 6 edges.
The edges number is *not reliable* and should be ignored. Some instances
include (some) edges in both directions, whilst others do not. Some files
contain loops (vertices adjacent to themselves), which should be ignored for
the clique problem.

The maximum clique in this file has size 3, with vertices 3, 4 and 5.

Other datasets are available, also using this format.

The maximum clique problem is equivalent to the maximum independent set problem
and the vertex cover problem. It is also used as an intermediate step in
solving the maximum common subgraph problem.
