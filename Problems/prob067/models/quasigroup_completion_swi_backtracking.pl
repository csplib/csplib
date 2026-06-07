% Example instance: a partially pre-filled 7x7 quasigroup (_ = empty cell).
% Passed to the solver as an argument (see quasigroup/2) so the same relation
% completes any board, mirroring how the MiniZinc references take the board as
% data rather than wiring it into the model.
partially_quasigroup([
    [1, _, _, 4, _, _, _],
    [_, 3, _, _, _, 7, _],
    [_, _, 5, _, _, _, 2],
    [4, _, _, 7, _, _, _],
    [_, 6, _, _, 2, _, _],
    [_, _, 1, _, _, 4, _],
    [7, _, _, 3, _, _, 6]
]).

% quasigroup(+Puzzle, -Solution): completes the partially filled Puzzle into
% Solution. Solution shares the grid with Puzzle, so empty cells get bound here.
quasigroup(Puzzle, Solution):-
    Solution = Puzzle,
    length(Solution, M),
    numlist(1, M, Domain),
    maplist(fill_row_dc(Domain, Solution), Solution).


% fill_row_dc(+Domain, +Solution, +Row): assign the empty cells of Row
fill_row_dc(Domain, Solution, Row):-
    include(nonvar, Row, Prefilled), % collect prefilled values
    subtract(Domain, Prefilled, Reduced),  % remove prefilled from domain
    assign_cell_dc(Row, Reduced, 1, Solution).

% assign_cell_dc(+Row, +Remaining, +Index, +Solution): fill each cell of Row from Remaining
assign_cell_dc([], _Remaining, _Index, _Solution).
assign_cell_dc([Cell|Rest], Remaining, Index, Solution):-
    nonvar(Cell), % cell pre-filled
    NextIndex is Index + 1,
    assign_cell_dc(Rest, Remaining, NextIndex, Solution).

assign_cell_dc([Cell|Rest], Remaining, Index, Solution) :-
    var(Cell),
    maplist(nth1(Index), Solution, Col),      % extract the column for the given index
    include(nonvar, Col, ColUsed),            % collect already filled column values
    subtract(Remaining, ColUsed, Candidates), % candidates = row domain minus column values
    member(Cell, Candidates),                 % cell empty -> assign from restricted domain
    select(Cell, Remaining, NewRemaining),    % update remaining row values
    NextIndex is Index + 1,
    assign_cell_dc(Rest, NewRemaining, NextIndex, Solution).
