% Quasigroup with Holes (aka Latin Square Completion)

include "alldifferent.mzn";

int: N;
int: digs = ceil(log(10.0,int2float(N))); % digits for output
set of int: PuzzleRange = 1..N;

array[1..N,1..N] of 0..N: start; %% initial board 0 = empty
array[1..N,1..N] of var PuzzleRange: puzzle;

% fill initial board
constraint forall(i,j in PuzzleRange)(
  if start[i,j] > 0 then puzzle[i,j] = start[i,j] else true endif );

% All different in rows
constraint forall (i in PuzzleRange) (
                   alldifferent( [ puzzle[i,j] | j in PuzzleRange ] ) );

% All different in columns.
constraint forall (j in PuzzleRange) (
                   alldifferent( [ puzzle[i,j] | i in PuzzleRange ] ) );

solve satisfy;

output [ show_int(digs,puzzle[i,j]) ++ " " ++
        if j == N /\ i != N then "\n" else "" endif
        | i,j in PuzzleRange ] ++ ["\n"];
