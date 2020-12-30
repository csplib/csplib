% Maximum Clique Problem (CSPLib 074)
% Very naive model. This is not a good way of solving it, but it is easy to
% understand.
%
% Ciaran McCreesh, c.mccreesh.1@research.gla.ac.uk
% 2015-09-05

% --- instance, specified as an adjacency matrix, which must be symmetric, using ---
% --- 0s and 1s rather than true and false because it looks prettier             ---
int: n = 5;
array[1..n, 1..n] of int: adj =
    [| 0, 1, 0, 1, 0
     | 1, 0, 1, 0, 0
     | 0, 1, 0, 1, 1
     | 1, 0, 1, 0, 1
     | 0, 0, 1, 1, 0 |];

% --- naive model ---

% decision variables: which vertices are in the clique?
array[1..n] of var bool: c;
var int: size;

% how many vertices have we selected?
constraint size = sum(c);

% we can only pick one of any non-adjacent pair of vertices
constraint forall (i, j in 1..n where i < j /\ 0 == adj[i,j]) (bool2int(c[i]) + bool2int(c[j]) <= 1);

solve maximize size;

