---
Title: Killer Sudoku
Proposer: Peter Nightingale
Category: Games and puzzles
---

Killer Sudoku is a puzzle played on a $\\{9\times 9\\}$ grid containing 81 cells. 
The cells are filled in with numbers from the set $\\{1\ldots 9\\}$.
Each row and column must contain all numbers $\\{1\ldots 9\\}$. Each of the 9
non-overlapping $3\times 3$ subsquares (named *boxes*) must also contain all numbers $\\{1\ldots 9\\}$.

Each Killer Sudoku puzzle has a set of *cages*. A cage is a set of contiguous cells
and a total; the numbers in the cells must add up to the total. Also, the cells in
a cage cannot contain the same number more than once. The cages do not overlap, 
and they cover all cells. Cages typically contain two to four cells. 
Typically a Killer Sudoku puzzle will have exactly one solution. 

An example Killer Sudoku puzzle is shown below. Each cage is shown as an area of one colour.

<center>
<figure>
  <img src="assets/Killersudoku_color.svg" alt="Killer Sudoku Puzzle">
  <figcaption>A Killer Sudoku Puzzle (public domain image from Wikipedia)</figcaption>
</figure>
</center>

The solution of the above puzzle is shown below. 

<center>
<figure>
  <img src="assets/Killersudoku_color_solution.svg" alt="Killer Sudoku Puzzle Solution">
  <figcaption>A Killer Sudoku Puzzle Solution (public domain image from Wikipedia)</figcaption>
</figure>
</center>


Generalisation to $n \times n$ grids
------

There is a straightforward generalisation of Killer Sudoku. For any $n$ that has
an integer square root, we have an $n \times n$ grid and each cell takes any
value in $\\{1\ldots n\\}$. In a solution each row and column contains all numbers $\\{1\ldots n\\}$,
and the $n$ non-overlapping $\sqrt{n} \times \sqrt{n}$ boxes also contain all
numbers $\\{1\ldots n\\}$. Cages function in the same way in the generalised 
problem as in the $\\{9\times 9\\}$ problem. 

