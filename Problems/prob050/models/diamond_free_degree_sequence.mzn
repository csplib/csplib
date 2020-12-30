% 
% Diamond-free Degree Sequence (CSPLib #50) in MiniZinc.
% 
% http://www.csplib.org/Problems/prob050/
% """
%  Proposed by Alice Miller, Patrick Prosser
%
% Given a simple undirected graph G=(V,E), where V is the set of vertices and E the set of 
% undirected edges, the edge {u,v} is in E if and only if vertex u is adjacent to vertex v∈G. 
% The graph is simple in that there are no loop edges, i.e. we have no edges of the form {v,v}. 
% Each vertex v∈V has a degree dv i.e. the number of edges incident on that vertex. Consequently 
% a graph has a degree sequence d1,…,dn, where di>=di+1. A diamond is a set of four vertices 
% in V such that there are at least five edges between those vertices. Conversely, a graph is 
% diamond-free if it has no diamond as an induced subgraph, i.e. for every set of four vertices 
% the number of edges between those vertices is at most four.
% 
% In our problem we have additional properties required of the degree sequences of the graphs, 
% in particular that the degree of each vertex is greater than zero (i.e. isolated vertices 
% are disallowed), the degree of each vertex is modulo 3, and the sum of the degrees is 
% modulo 12 (i.e. |E| is modulo 6).
%
% The problem is then for a given value of n, produce all unique degree sequences d1,…,dn such 
% that
%
%  * di≥di+1
%  * each degree di>0 and di is modulo 3
%  * the sum of the degrees is modulo 12
%  * there exists a simple diamond-free graph with that degree sequence
%
% Below, as an example, is the unique degree sequence forn=10 along with the adjacency matrix of 
% a diamond-free graph with that degree sequence.
%
%   n = 10
%   6 6 3 3 3 3 3 3 3 3
%
%   0 0 0 0 1 1 1 1 1 1 
%   0 0 0 0 1 1 1 1 1 1 
%   0 0 0 0 0 0 0 1 1 1 
%   0 0 0 0 1 1 1 0 0 0 
%   1 1 0 1 0 0 0 0 0 0 
%   1 1 0 1 0 0 0 0 0 0 
%   1 1 0 1 0 0 0 0 0 0 
%   1 1 1 0 0 0 0 0 0 0 
%   1 1 1 0 0 0 0 0 0 0 
%   1 1 1 0 0 0 0 0 0 0
%
% """

%
% Note: Most FlatZinc solver yields all solutions of x even when  
%       degree is the only in output.
% 
% The exception is the Gecode solver, which just prints the 
% distinct degrees proviso:
%  - only degrees is in the output
%  - only degrees is in the labeling (or "solve satisfy")

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 11;

% decision variables
array[1..n,1..n] of var 0..1: x;
array[1..n] of var 1..n: degrees;


% solve satisfy;
solve :: int_search(degrees, first_fail, indomain_split, complete) satisfy;

constraint
  forall(i,j,k,l in 1..n where i < j /\ j < k /\ k < l) (
     x[i,j] + x[i,k] + x[i,l] + x[j,k] + x[j,l] + x[k,l] <= 4
  )
  /\
  forall(i in 1..n) (
     degrees[i] = sum([x[i,j] | j in 1..n])
     /\ degrees[i] mod 3 = 0
     % no loops
     /\ x[i,i] = 0
  ) 
  /\ % undirected graph
  forall(i,j in 1..n) (
    x[i,j] = x[j,i]
  )
  /\ sum(degrees) mod 12 = 0

  % symmetry breaking
  /\ decreasing(degrees)
  /\ lex2(x)
;

output 
[ "degrees: ", show(degrees), "\n"]
% ++
% [
%   if j = 1 then "\n" else " " endif ++
%     show(x[i,j])
%   | i,j in 1..n
% ]
;

