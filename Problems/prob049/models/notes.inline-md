The problem can be easily modeled as a CSP problem with 2 sets of N/2 variables (called A\_{i} and B\_{i}, i in 1..N/2) whose initial domain are the set of integers {1..N}. The variables are constrained as follows:

* all variables are different
* $\Sigma_{i=1}^{N/2} A_i = \Sigma_{i=1}^{N/2} B_i = N * (N+1) / 2 / 2$
* $\Sigma_{i=1}^{N/2} A_i^2 = \`Sigma_`{i=1}^{N/2} B_i^2 = N * (N+1) * (2*N+1) / 6 / 2$


To break symmetries (avoiding permutations of a group, and swapping the two groups) it is better to add these redundant constraints:

* $A_1 < A_2 < ... < A_{N/2}$
* $B_1 < B_2 < ... < B_{N/2}$
* $A_1 = 1$ (the value 1 is always in the first group))

A good heuristic consists in iteratively placing the biggest missing value (thus in descending order). NB: doing so the "all different" constraint is not needed (only different values are assigned one by one).

If only the first solution is wanted then it is further preferable to first try to put this value in the set which has the smallest sum (sum of already placed values). If all solutions are wanted this value will also be tried in the other group so the test on the sums can involve a little overhead.
