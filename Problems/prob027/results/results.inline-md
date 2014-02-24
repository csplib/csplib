For the 4x4 problem with 3 colours, the puzzle with largest least solution (in the sense outlined in the specification) needs 10 clicks to solve. There are 19 different instances needing 10 clicks, up to the symmetries described below.

We have not yet attempted other instances, although some others have been solved using purely algebraic methods.

The constraints are hard to express directly without the use of algebraic methods. Contact Ian Gent for details.

The NxN problem with N-1 colours always provides a test instance, because we can click 1 time in the squares in the top row, and -1 == N-2 times in the second row, to generate a similar set of constraints to the 4x4 problem.

The problem always has a high degree of symmetry, with N! row permutations, N! column permutations, and a reflection in a diagonal, for N! x N! x 2 symmetries. These can be exploited to reduce search.

Ian Gent, Steve Linton and Barbara Smith have solved the N=4 c=3 problem using an ILOG Solver program partly output by a GAP program. Without the use of symmetry, it needed 866 fails and 31sec to find the optimal solution, and 57664 fails and 2324 secs to prove optimality. With symmetry breaking, these four numbers were reduced to 116 fails, 18.6 secs, and 499 fails and 56.6 secs. 

