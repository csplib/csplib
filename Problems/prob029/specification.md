Title:    Prime queen attacking problem
Proposer: Christian Bessiere
Category: Combinatorial mathematics
          Games and puzzles


This problem, posed first by G.L. Honaker, is to put a queen and the $n^2$ numbers $1,...,n^2$, on a $n \times n$ chessboard so that:

1. no two numbers are on the same cell,
2. any number $i+1$ is reachable by a knight move from the cell containing $i$,
3. the number of "free" primes (i.e., primes not attacked by the queen) is minimal.

Note that 1 is not prime, and that the queen does not attack its own cell.


An Example of solution
----------------------

A 6x6 chessboard without free primes (the queen is on the cell containing 33): 


   |    |    |    |     |	
 --| ---| -- | ---|  -- |  --
 9 | 32 |  3 | 28 | 11 | 30
 4 | 27 | 10 | 31 | 34 |  1
17 |  8 | 33 |  2 | 29 | 12
26 |  5 | 16 | 19 | 22 | 35
15 | 18 |  7 | 24 | 13 | 20
 6 | 25 | 14 | 21 | 36 | 23