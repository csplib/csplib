:- use_module(library(clpfd)).

% Example instance: a partially pre-filled 7x7 quasigroup (_ = empty cell).
% The solver takes the puzzle as an argument (see quasigroup/2), so the same
% relation completes any board. This fact is just one concrete instance to run
% it on, mirroring how the MiniZinc references pass the board in as data.
partially_quasigroup([
    [1, _, _, 4, _, _, _],
    [_, 3, _, _, _, 7, _],
    [_, _, 5, _, _, _, 2],
    [4, _, _, 7, _, _, _],
    [_, 6, _, _, 2, _, _],
    [_, _, 1, _, _, 4, _],
    [7, _, _, 3, _, _, 6]
]).

% quasigroup(+Puzzle, -Solution): completes the partially filled Puzzle into a
% full Latin square. The holes in Puzzle are unbound variables, so unifying
% Solution with Puzzle lets the constraints fill them in place.
quasigroup(Puzzle, Solution):-
    Solution = Puzzle,
    length(Solution, M),
    append(Solution, Vars), % flatten the matrix into a single list and restrict every cell (every element in Vars) to 1-M.
    Vars ins 1..M,

    maplist(all_distinct, Solution), % all elements in a row must be distinct
    transpose(Solution, Transposed),
    maplist(all_distinct, Transposed), % all elements in a col must be distinct

    label(Vars).
