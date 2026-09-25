% APPROACH: Backtracking left-to-right with domain restriction PER CELL.
% Before assigning a value, candidates are computed as the intersection of:
%   - values not yet used in the same row, column, and 3x3 block
%   - values still valid for the cage (not already used in cage, compatible with remaining sum)

% THE PUZZLE: Unknown cells are represented as anonymous variables (_).
puzzle([
    [2, _, 5, _, _, _, _, _, _],
    [_, 6, _, _, _, _, _, _, _],
    [_, 9, _, _, _, _, _, _, _],
    [_, _, _, 2, _, _, _, _, _],
    [1, _, _, _, _, _, _, _, _],
    [9, 7, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _]
]).

cages([
    cage(3,  [pos(1,1), pos(1,2)]),
    cage(15, [pos(1,3), pos(1,4), pos(1,5)]),
    cage(22, [pos(1,6), pos(2,6), pos(2,5), pos(3,5)]),
    cage(4,  [pos(1,7), pos(2,7)]),
    cage(16, [pos(1,8), pos(2,8)]),
    cage(15, [pos(1,9), pos(2,9), pos(3,9), pos(4,9)]),
    cage(25, [pos(2,1), pos(3,1), pos(3,2), pos(2,2)]),
    cage(17, [pos(2,3), pos(2,4)]),
    cage(9,  [pos(3,3), pos(3,4), pos(4,4)]),
    cage(8,  [pos(3,6), pos(4,6), pos(5,6)]),
    cage(20, [pos(3,8), pos(3,7), pos(4,7)]),
    cage(6,  [pos(4,1), pos(5,1)]),
    cage(14, [pos(4,2), pos(4,3)]),
    cage(17, [pos(4,5), pos(5,5), pos(6,5)]),
    cage(17, [pos(5,7), pos(5,8), pos(4,8)]),
    cage(13, [pos(5,2), pos(6,2), pos(5,3)]),
    cage(20, [pos(5,4), pos(6,4), pos(7,4)]),
    cage(20, [pos(6,6), pos(7,6), pos(7,7)]),
    cage(12, [pos(5,9), pos(6,9)]),
    cage(27, [pos(6,1), pos(7,1), pos(8,1), pos(9,1)]),
    cage(6,  [pos(6,3), pos(7,2), pos(7,3)]),
    cage(6,  [pos(6,7), pos(6,8)]),
    cage(10, [pos(7,5), pos(8,5), pos(8,4), pos(9,4)]),
    cage(14, [pos(7,8), pos(7,9), pos(8,8), pos(8,9)]),
    cage(8,  [pos(8,2), pos(9,2)]),
    cage(16, [pos(8,3), pos(9,3)]),
    cage(15, [pos(8,6), pos(8,7)]),
    cage(13, [pos(9,5), pos(9,6), pos(9,7)]),
    cage(17, [pos(9,8), pos(9,9)])
]).

% sudoku(-Solution): Solution is the filled-in 9x9 grid
sudoku(Solution) :-
    puzzle(Solution),
    fill_grid(Solution, Solution, 1).

% fill_grid(+Rows, +Solution, +RowIdx)
% Iterates row by row, tracking RowIdx
fill_grid([], _, _).
fill_grid([Row|RestRows], Solution, RowIdx) :-
    fill_row(Row, RowIdx, 1, Solution), % Goes through one row
    NextRow is RowIdx + 1,
    fill_grid(RestRows, Solution, NextRow).

% fill_row(+Cells, +RowIdx, +ColIdx, +Solution)
% Gets called, if cell is already prefilled
fill_row([], _, _, _).
fill_row([Cell|Rest], RowIdx, ColIdx, Solution) :-
    nonvar(Cell),
    NextCol is ColIdx + 1,
    fill_row(Rest, RowIdx, NextCol, Solution).

% Gets called if cell is empty. Restrics domain
fill_row([Cell|Rest], RowIdx, ColIdx, Solution) :-
    var(Cell),
    candidates(RowIdx, ColIdx, Solution, Cands), % Candidates left due to row, col, box and cage constraint
    member(Cell, Cands),
    NextCol is ColIdx + 1,
    fill_row(Rest, RowIdx, NextCol, Solution).

% candidates(+RowIdx, +ColIdx, +Solution, -Cands)
% Candidates = intersection of row/col/block domain and cage domain
candidates(RowIdx, ColIdx, Solution, Cands) :-
    % row domain
    nth1(RowIdx, Solution, Row),
    include(nonvar, Row, RowUsed),
    % col domain
    maplist(nth1(ColIdx), Solution, Col),
    include(nonvar, Col, ColUsed),
    % block domain
    block_values(RowIdx, ColIdx, Solution, BlockUsed),
    cage_candidates(RowIdx, ColIdx, Solution, CageCands),
    numlist(1, 9, All),
    subtract(All, RowUsed, After1),
    subtract(After1, ColUsed, After2),
    subtract(After2, BlockUsed, After3),
    intersection(After3, CageCands, Cands).

% Returns values still valid for this cell from the cage perspective:
% not already used in the cage, and compatible with the remaining sum
% cage_candidates(+RowIdx, +ColIdx, +Solution, -CageCands)
cage_candidates(RowIdx, ColIdx, Solution, CageCands) :-
    cages(Cages),
    find_cage(pos(RowIdx, ColIdx), Cages, cage(Sum, Positions)),
    maplist(pos_to_val(Solution), Positions, AllVals),
    include(nonvar, AllVals, FilledVals), % keep only occupied values
    sum_list(FilledVals, PartialSum), % sum up all values in the cage
    length(Positions, Total), % Total = num of cells in the cage
    length(FilledVals, Filled), % Filled = num of filled cells in cage
    RemainingCells is Total - Filled - 1, % Remaining cells without current cell
    RemainingSum is Sum - PartialSum, % RemainingSum = value to spread across the empty cells
    numlist(1, 9, All),
    subtract(All, FilledVals, WithoutUsed), % excludes the already used values
    % include goes through every value in the list WithoutUsed, tests it against the predicate, and stores the one where the predicate returns true in the CageCands list
    include(valid_cage_val(RemainingCells, RemainingSum), WithoutUsed, CageCands). % include goes through every value in the list WithoutUsed, tests it against the predicate, and stores the one where the predicate returns true in the CageCands list

% valid_cage_val(+Remaining, +RemainingSum, +V)
% last cell in cage: must equal remaining sum exactly
valid_cage_val(0, RemainingSum, V) :-
    V =:= RemainingSum.

% Not last cell: V must leave an achievable remaining sum for other cells.
% Gauss Law: min sum of remaining distinct values = 1+2+3+..+N = Remaining * (Remaining + 1) // 2
%            max sum of Remaining distinct values = 9+8+7+..+N = Remaining * (9 + (9 - Remaining + 1)) // 2
valid_cage_val(Remaining, RemainingSum, V) :-
    Remaining > 0,
    MinSum is Remaining * (Remaining + 1) // 2,
    MaxSum is Remaining * (19 - Remaining) // 2,
    V =< RemainingSum - MinSum, % all other cells must have at least the lowest numbers
    V >= RemainingSum - MaxSum.  % all other cells can have at most the highest numbers

% find_cage(+Pos, +Cages, -Cage)
% Walks through the cage list and returns the cage containing Pos
find_cage(Pos, [cage(Sum, Positions)|_], cage(Sum, Positions)) :-
    member(Pos, Positions), !. % Checks with member whether Pos is in the current cage's positions, ! stops the search once the cage is found
find_cage(Pos, [_|Rest], Cage) :-
    find_cage(Pos, Rest, Cage).

% pos_to_val(+Solution, +Pos, -Val)
% Translates pos(R,C) to the current cell value
pos_to_val(Solution, pos(R, C), Val) :-
    nth1(R, Solution, Row),
    nth1(C, Row, Val).

% block_values(+RowIdx, +ColIdx, +Solution, -Values)
% Collects all filled values in the 3x3 block containing (RowIdx, ColIdx)
block_values(RowIdx, ColIdx, Solution, Values) :-
    % calculate where the block starts: e.g. Cell (5,7) starts at (4,7) (upper left corner)
    BlockRow is ((RowIdx - 1) // 3) * 3 + 1,
    BlockCol is ((ColIdx - 1) // 3) * 3 + 1,
    R2 is BlockRow + 1, R3 is BlockRow + 2,
    C2 is BlockCol + 1, C3 is BlockCol + 2,
    nth1(BlockRow, Solution, Row1),
    nth1(R2, Solution, Row2),
    nth1(R3, Solution, Row3),
    nth1(BlockCol, Row1, V1), nth1(C2, Row1, V2), nth1(C3, Row1, V3),
    nth1(BlockCol, Row2, V4), nth1(C2, Row2, V5), nth1(C3, Row2, V6),
    nth1(BlockCol, Row3, V7), nth1(C2, Row3, V8), nth1(C3, Row3, V9),
    include(nonvar, [V1,V2,V3,V4,V5,V6,V7,V8,V9], Values).
