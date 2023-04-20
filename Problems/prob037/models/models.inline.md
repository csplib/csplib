##Notes on modelling##

Peg Solitaire is essentially a planning problem: the goal is to find a sequence of actions that transform the initial state into the goal state. We are helped by the fact that we know exactly how many moves are necessary: (the number of pegs in the initial configuration - the number of pegs in the goal configuration). A CP model found to be successful in [1] employs a 1-dimensional array of variables, moves[t], which records the move made at time-step t. The domain of each element of this array is the set of possible moves, i.e. all ways of removing a peg from the board. The number of possibilities will vary according to the board shape, but the English board has 76 such possible moves.

A second array, bState[i, j, t] of 01 variables (where i, j specify a board position and t is the time-step), is used to keep track of the state of the board before and after each move. This array is used also to specify the three pre-conditions (two pegs and a hole) and three post-conditions (two holes and a peg) of each possible move. 

### Symmetry ###
Peg Solitaire contains a lot of symmetry. Depending on the shape of the board, rotation and reflection symmetries will usually apply. There are also symmetries of independent moves: that is, entries in moves[] that can be exchanged without affecting the solution. Symmetries of pairs of independent moves can be broken by imposing an ordering on moves[] as follows. Symmetries of larger groups of independent moves are more expensive to break.

independent(moves[i], moves[i+1]) -> moves[i] <= moves[i+1]

There are also symmetric paths to the same board state. On some occasions this is due to independent moves, but on others disjoint sets of moves can lead to the same position. These symmetries can be broken by identifying the symmetric paths and adding constraints to allow only one representative from each equivalence class, but the identification process itself is expensive. 

### Other models ###
As described in cite{jefferson03} a PDDL model suitable for use with AI planning systems can easily be created, as can an integer programming model.

### Fool's Solitaire ###
An optimisation variant of peg Solitaire is to attempt to reach a position where no further moves are possible in the shortest sequence of moves. This can be modelled by adding a special 'DeadEnd' move to the domain of each variable in moves[]. This move is only applicable when no other move is possible. The problem is then to maximise the occurrences of 'DeadEnd' in moves[]. 

## Code ##
A basic Ilog Solver program to solve central Solitaire can be found [here](SolitaireSolverCode.zip).

For comparison, a PDDL version of the problem,
for use with AI planning systems such as
[Blackbox](https://www.cs.rochester.edu/u/kautz/satplan/blackbox/),
[Fastforward](https://fai.cs.uni-saarland.de/hoffmann/ff.html),
[HSP](https://planning.wiki/ref/planners/hsp), or
[STAN](https://www.cs.cmu.edu/afs/cs/project/jair/pub/volume17/howe02a-html/node11.html),
can be found [here](SolitairePDDL.zip).
