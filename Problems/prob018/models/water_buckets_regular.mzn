% 
% MiniZinc model for the water buckets problem
%
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

%
% Solution should be
%   x: [1, 9, 10, 11, 12, 13, 14, 15]
%   8,0,0 -> 3,5,0
%   3,5,0 -> 3,2,3
%   3,2,3 -> 6,2,0
%   6,2,0 -> 6,0,2
%   6,0,2 -> 1,5,2
%   1,5,2 -> 1,4,3
%   1,4,3 -> 4,4,0
% 
include "globals.mzn";

int: n_states = 15;
int: input_max = 15;
int: initial_state = 1;
set of int: accepting_states = {15};


% distance
array[1..n_states, 1..n_states] of 0..input_max: transition_fn =
array2d(1..n_states, 1..n_states,
[%1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
  0, 2, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, % 1
  0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, % 2 
  0, 0, 0, 4, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, % 3
  0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, % 4
  0, 0, 0, 0, 0, 6, 0, 0, 9, 0, 0, 0, 0, 0, 0, % 5
  0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, % 6
  0, 0, 0, 0, 0, 0, 0, 8, 9, 0, 0, 0, 0, 0, 0, % 7
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15, % 8 
  0, 0, 0, 0, 0, 0, 0, 0, 0,10, 0, 0, 0, 0, 0, % 9
  0, 2, 0, 0, 0, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, %10
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12, 0, 0, 0, %11 
  0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,13, 0, 0, %12
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,14, 0, %13 
  0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15, %14
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,15, %15
]);


array[1..n_states] of string:  nodes = [
        "8,0,0", % 1 start
        "5,0,3", % 2
        "5,3,0", % 3 
        "2,3,3", % 4 
        "2,5,1", % 5
        "7,0,1", % 6
        "7,1,0", % 7
        "4,1,3", % 8
        "3,5,0", % 9
        "3,2,3", % 10
        "6,2,0", % 11
        "6,0,2", % 12
        "1,5,2", % 13
        "1,4,3", % 14
        "4,4,0"  % 15 goal
        ];


array[1..input_max] of var 0..input_max: x;
var 0..input_max: cost;

% solve satisfy;
solve minimize cost;

constraint
regular(x, n_states, input_max, transition_fn,
        initial_state, accepting_states)
;

constraint
   cost = 2+sum([bool2int(x[i-1] != x[i] ) | i in 2..input_max])
;

output 
["cost: " ++ show(cost) ++ "\n"] ++
[show(initial_state) ++ " "] ++
[
  if fix(x[i]) < input_max then show(x[i]) ++ " " else " " endif
  | i in 1..input_max where fix(x[i]) < input_max
] ++ 
[show(input_max) ++ "\n"] ++ 
["\n\n"] ++

[show(nodes[initial_state]) ++ "\n"] ++
[
  if fix(x[i]) < input_max then show(nodes[fix(x[i])]) ++ "\n" else " " endif
  | i in 1..input_max where fix(x[i]) < input_max
] ++ 
[show(nodes[input_max]) ++ "\n"] ++ 
["\n"];

