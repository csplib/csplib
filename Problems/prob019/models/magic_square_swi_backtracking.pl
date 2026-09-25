% Magic Square backtracking solver.
% Fills an NxN grid with values 1..N^2 such that all rows, columns,
% and both diagonals sum to the magic sum S = N*(N^2+1)/2.

% magic_square_bt(+N, -Square)
magic_square_bt(N, Square) :-
    length(Square, N),
    maplist(length_(N), Square),      % create NxN matrix of variables
    append(Square, Vars),             % flatten matrix to a single list
    Max is N * N,
    numlist(1, Max, Domain),          % domain = [1..N^2]
    Sum is N * (Max + 1) // 2,       % magic sum
    fill(Vars, Domain, Square, N, Sum).

% fill(+Vars, +Remaining, +Square, +N, +Sum)
fill([], _, _, _, _).
fill([H|Rest], Remaining, Square, N, Sum) :-
    select(H, Remaining, NewRemaining), % pick an unused value (guarantees all_distinct)
    check_partial(Square, N, Sum),      % check constraints after every assignment
    fill(Rest, NewRemaining, Square, N, Sum).

% Checks rows, columns, and diagonals after each cell assignment.
% check_partial(+Square, +N, +Sum)
check_partial(Square, N, Sum) :-
    maplist(check_line_partial(Sum), Square),       % row sums
    numlist(1, N, Indices),
    maplist(check_col_partial(Square, Sum), Indices), % column sums
    check_diags_partial(Square, N, Sum).            % diagonal sums

% Partial line check (row or column):
% If complete: partial sum must equal Sum exactly.
% If incomplete: partial sum must not yet exceed Sum.
% check_line_partial(+Sum, +Line)
check_line_partial(Sum, Line) :-
    include(nonvar, Line, Filled),     % only already assigned cells
    sum_list(Filled, PartialSum),
    length(Line, Total),
    length(Filled, FilledCount),
    FilledCount < Total,               % line incomplete: partial sum must not exceed Sum
    PartialSum < Sum.

check_line_partial(Sum, Line) :-
    include(nonvar, Line, Filled),     % only already assigned cells
    sum_list(Filled, PartialSum),
    length(Line, Total),
    length(Filled, Total),             % line complete: FilledCount equals Total
    PartialSum =:= Sum.

% check_col_partial(+Square, +Sum, +Index)
check_col_partial(Square, Sum, Index) :-
    maplist(nth1(Index), Square, Col), % extract column at given index
    check_line_partial(Sum, Col).

% Diagonal checks use the same partial line check.
% check_diags_partial(+Square, +N, +Sum)
check_diags_partial(Square, N, Sum) :-
    Last is N - 1,
    numlist(0, Last, Indices),
    maplist(diag1_cell(Square), Indices, Diag1),       % main diagonal
    check_line_partial(Sum, Diag1),
    maplist(diag2_cell(Square, N), Indices, Diag2),    % anti-diagonal
    check_line_partial(Sum, Diag2).

% ----------------
% HELPER FUNCTIONS

% length_(+N, ?List): List is a list of N elements
length_(N, List) :- length(List, N).

% diag1_cell(+Square, +I, -Cell): Cell is the (I,I) main-diagonal cell
diag1_cell(Square, I, Cell) :-
    nth0(I, Square, Row),   % row I
    nth0(I, Row, Cell).     % cell I in that row -> main diagonal

% diag2_cell(+Square, +N, +I, -Cell): Cell is the (I,N-1-I) anti-diagonal cell
diag2_cell(Square, N, I, Cell) :-
    nth0(I, Square, Row),   % row I
    J is N - 1 - I,         % mirrored column index
    nth0(J, Row, Cell).     % anti-diagonal
