% Quasigroup with Holes (aka Latin Square Completion)
 
int: N;
int: digs = ceil(log(10.0,int2float(N))); % digits for output
set of int: PuzzleRange = 1..N;
 
array[1..N,1..N] of 0..N: start; %% initial board 0 = empty
array[1..N,1..N, 1..N] of var 0..1: puzzle;
 
% fill initial board
constraint forall(i,j in PuzzleRange)(
  if start[i,j] > 0 then puzzle[i,j, start[i,j]] = 1 else true endif );
 
% All different in rows
constraint forall (i,k in PuzzleRange) (
        sum(j in PuzzleRange)(puzzle[i,j,k]) == 1
);

% All different in columns
constraint forall (j,k in PuzzleRange) (
        sum(i in PuzzleRange)(puzzle[i,j,k]) == 1
);

% Each cell must have a number assigned
constraint forall (i,j in PuzzleRange) (
        sum(k in PuzzleRange)(puzzle[i,j,k]) == 1
);

solve satisfy;
 
output [ show_int(digs,puzzle[i,j]) ++ " " ++
        if j == N /\ i != N then "\n" else "" endif
        | i,j in PuzzleRange ] ++ ["\n"];