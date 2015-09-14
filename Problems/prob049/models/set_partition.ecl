/*

  Set partition problem in ECLiPSe.

  Problem formulation from
    http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
   This is a partition problem.
   Given the set S = {1, 2, ..., n}, 
   it consists in finding two sets A and B such that:
   <ul>
   <li>A U B = S,</li>
   <li>|A| = |B|,</li>
   <li>sum(A) = sum(B),</li>
   <li>sum_squares(A) = sum_squares(B).</li>
   </ul>
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_partition.mzn
  * Gecode/R: http://www.hakank.org/gecode_r/set_partition.rb
  * Comet   : http://www.hakank.org/comet/set_partition.co


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

  Model simplified by Joachim Schimpf

*/

% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/

:-lib(ic).
:-lib(ic_sets).


% find all (7) solutions for N = 16.
go :-
        N = 16,
        NumSets = 2,
        set_partition(N, NumSets, Sets, Sums),
        writeln(sets:Sets), writeln(sums:Sums), nl,
        fail.

%
% check for a solution between N = 17 and 32
%
go2 :-
        ic : '::'(N,17..32),
        indomain(N),
        NumSets = 2,
        writeln(n:N),
        set_partition(N, NumSets, Sets, Sums),
        writeln(sets:Sets), writeln(sums:Sums), nl.


%
% Here we find the minimal N and NumSets for
% a solution to the problem.
%
go3 :-
        N #:: 2..20,
        NumSets #:: 3..9,
        indomain(N),
        indomain(NumSets),
        writeln([n:N,num_sets:NumSets]),
        set_partition(N,NumSets).

        

set_partition(N, NumSets, Sets, [Sum,SumSquared]) :-

        % create list of sets
        intsets(Sets,NumSets,1,N),

        % create the universe for partition_set
        % and the weights for weight/3 below.
        dim(Weights,[N]),
        dim(Weights2,[N]),
        ( for(I,1,N), foreach(I,Universe),
          foreacharg(I,Weights), foreacharg(W2,Weights2) do
            W2 is I*I
        ),

        % Sets must be a partition of the Universe
        partition_set(Sets, Universe),

        % all sets must have the same cardinality _C
        ( foreach(Set,Sets), param(_C) do
            #(Set, _C)
        ),

        % all sums and all squared sums must be equal
        ( foreach(Set,Sets), param(Weights,Weights2,Sum,SumSquared) do
            weight(Set, Weights, Sum),
            weight(Set, Weights2, SumSquared)
        ),

        % symmetry breaking
        [FirstSet|_] = Sets,
        1 in FirstSet,

        %
        % search
        % 
        label_sets(Sets).



%
% labeling the sets
%
label_sets([]).
label_sets([S|Ss]) :-
        insetdomain(S,increasing,big_first,in_notin),
        label_sets(Ss).


%
% Partitions the list of sets S into disjoint sets.
% All elements in the universe Universe must be
% included in exactly one of the sets.
%
partition_set(S, Universe) :-
        all_disjoint(S),
        all_union(S,Universe).

