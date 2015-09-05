---
Title:    Distance-Based Constrained Clustering
Proposer: Thi-Bich-Hanh Dao
Category: Clustering
---

Cluster Analysis is a Data Mining task that aims at partitioning a given set of objects into clusters,
such that the objects inside the same cluster
are similar, while being different from the objects belonging to other clusters. We consider a dataset of  objects and a dissimilarity measure between any two objects.
The homogeneity of the cluster is usually expressed by an optimization criterion, which can be among other:

* Maximizing the minimal split between clusters, the minimal split between clusters is the 
smallest dissimilarity between two objects of different clusters;
* Minimizing the maximal diameter of clusters, the maximal diameter is the largest dissimilarity 
between two objects in the same cluster;
* Minimizing the within-cluster sum of dissimilarities;
* Minimizing the within-cluster sum of squares:  in a Euclidean space
the within-cluster sum of squares is the sum of the squared Euclidean distances between each object 
and the centroid of the cluster containing the object.
* etc.

User previous knowledge can be integrated to clustering, which leads to Constrained Clustering. User constraints can be instance-level constraints or cluster-level constraints. Instance-level constraints are must-link or cannot-link constraints, which state that two objects must be or cannot be in the same cluster. Cluster-level constraints state requirements on the size, the diameter, the density, etc. of the clusters.
All of the criteria except the split one are NP-Hard. The split criterion which is polynomial becomes NP-Hard with user constraints.

