---
Title:    The n-Fractions Puzzle
Proposer: 
    - Alan Frisch
    - Christopher Jefferson
    - Ian Miguel
    - Toby Walsh
Category: 
    - Combinatorial mathematics
    - Games and puzzles
---



# Original Specification

The original fractions puzzle is specified as follows. Find 9 distinct non-zero digits that satisfy:

```
A    D    G
-- + -- + -- == 1
BC   EF   HI
```

where `BC` is shorthand for `10B+C`, `EF` for `10E+F` and `HI` for `10H+I`.


# n-Fractions

A simple generalisation is as follows. Find 3n non-zero digits satisfying: $ 1 = \sum_{i \in 1..n} x_i / y_iz_i $

where $y_iz_i$ is shorthand for $10y_i+z_i$ and the number of occurrences of each digit in $1..9$ is between $1$ and $ceil(n/3)$.

Since each fraction is at least $1/99$, this family of problems has solutions for at most $n <= 99$.
An interesting problem would be to find the greatest $n$ such that at least one solution exists.
A further generalisation might specify that the fractions sum to $ceil(n/3)$.

