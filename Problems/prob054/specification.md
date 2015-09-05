---
Title:    N-Queens
Proposer: Bilal Syed Hussain
Category: Games and puzzles
---

Overview
========

Can $n$ queens (of the same colour) be placed on a $n\times n$ chessboard so that none of the  queens can attack each other?

In chess a queen attacks other squares on the same row, column, or either diagonal as itself. So the $n$-queens problem is to find a set of $n$ locations on a chessboard, no two of which are on the same row, column or diagonal.  

<center>
<figure>
  <img src="assets/4queens.png" alt="solution to 4-queens">
  <figcaption>A solution to 4-queens</figcaption>
</figure>
</center>

A simple arithmetical observation may be helpful in modelling. Suppose a queen is represented by an ordered pair (α,β), the value α represents the queen’s column, and β its row on the chessboard. Then two queens do not attack each other iff they have different values of *all* of α, β, α-β, and α+β. It may not be intuitively obvious that chessboard diagonals correspond to sums and differences, but consider moving one square along the two orthogonal diagonals: in one direction the sum of the coordinates does not change, while in the other direction the difference does not change.

The problem is extremely well studied in the mathematical literature. An outstanding survey is available cite{Bell20091}.

Complexity
==========

Some care has to be taken when using the $n$-queens problem as a benchmark.  Here are some points to bear in mind:

* The $n$-queens problem is solvable for $n=1$ and $n \qeq 4$. So the decision problem is solvable in constant time. 
* On the other hand no 
