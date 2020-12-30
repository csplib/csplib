A possible encoding as a maxCSP:

**variables:**

a variable $V_i$ for each number i in $\\{1,...,n^2\\}$ and
a variable $Q$ for the queen.

**domains**:

the domains of all these variables are the possible cells on the n x n chessboard.

**hard constraints:**

an alldiff constraint on the $n^2$ $V_i$ variables.
for each pair $V_i, V_{i+1}$ of variables, a binary constraint allowing only the pairs of cells being the start and end of a knight move.

**soft constraints:**

for each prime $p$, a binary constraint involving $Q$ and $V_p$, allowing only the pairs of cells being the start and end of a queen move.

**goal:**

minimize the number of soft constraints violated.