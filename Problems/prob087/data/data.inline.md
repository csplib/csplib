The cross product of 

 - *w* in *{8,9,10,11,12,13,14,15,16}*
 - *M*: *M* is a $N^{4x7}$ matrix with, which is typically problem specific. In all cases however, the sum of each column
   of *M* must be equal to the number of employees which is equal to the number of weeks *w*. For test case generation we
   create *M* for example in the following way:
   $M_{i,j}$ = *w/4* (rounded up) if *i < w mod 4* else *w/4* (rounded down).
 - $s_{min}$ in *{2, 3}*
 - $s_{max}$ in *{3, 4}*

easily creates big instances of the problem.
By changing M, more instances can be created.

See the following sample instance Rostering-*8-M-2-4*, where *M* =

| Shift \ Day | Mo | Tu | We | Th | Fr | Sa | Su |
|-------------|---:|---:|---:|---:|---:|---:|---:|
| Day Off     | 2  | 2  | 2  | 2  | 2  | 4  | 4  |
| Early       | 2  | 2  | 2  | 2  | 2  | 2  | 2  |
| Late        | 2  | 2  | 2  | 2  | 2  | 1  | 1  |
| Night       | 2  | 2  | 2  | 2  | 2  | 1  | 1  |



