Title:    Alien Tiles Problem
Proposer: Ian Gent
Category: Games and puzzles

 The Alien Tiles puzzle is available for play over the internet at [www.alientiles.com](http://www.alientiles.com). We addressed the combinatorial problem of finding the most difficult puzzle, in a certain sense.

 For the Alien Tiles puzzle, you are presented with a square grid, with each square a given colour. Each grid square can be one of some number of colours, and the colours are ordered in a cycle, for example Red --> Green --> Blue --> Red. Moves in the puzzle are made by clicking on one of the grid squares. Each click rotates the colour by one in both the clicked square, and all other squares in the same row and column. Given a starting state and a goal state of the grid, the puzzle is to find a
 set of clicks of squares that achieves the goal.

 Notice that the order of the clicks does not matter, so all we have to do is to decide how many times to click on each square. The puzzle reduces to arithmetic modulo the number of colours, c. Clicking adds one mod c to each square in the same row and column. Instead of colours the start and goal states are an assignment of integers mod c to the squares. For simplicity we assume from now on that the start state is all zeroes.

 There are a number of interesting combinatorial questions about Alien Tiles. The straightforward one of solving given positions is in fact not hard, because the operations of clicking are commutative. We outline one particular question, whose solution is not currently known in general.

 With certain versions of the puzzle, if a goal state can be reached there are many equivalent ways of reaching it. For example, consider a 4x4 grid, with c=3 colours. Consider clicking once in each square in the top row, and twice in each square in the second row. This adds 3 clicks to each square in the bottom two rows, so makes no difference mod 3. It adds 6 clicks to each square in the top row, and 9 clicks to each square in the second row. The net result is that no difference mod 3 is made
 by this set of clicks, and the final colours in the squares of the grid are the same as the starting colours. We can do the same for any two rows, and two columns, and any combination thereof. Therefore there are many equivalent versions of any given solution. Of all the equivalence class of solutions under these operations, there is some solution with minimum number of clicks over the whole grid. We can now ask: what is the absolute maximum number of clicks necessary to solve any (solvable)
 goal state? That is, which solvable state has the largest minimum number of clicks in its equivalence class of solutions?
