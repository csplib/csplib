:- use_module(library(clpfd)).

% magic_square(+N, -Square)
magic_square(N, Square) :-
    % Matrix (Square): N rows
    length(Square, N), % Square has N rows
    maplist(length_(N), Square), % each row is a list of N variables

    % Collect all cells, set domain and uniqueness
    append(Square, Vars), % creates a single list from the matrix
    Max is N * N, % highest value (n squared)
    Vars ins 1..Max, % every cell must be between 1 and Max
    all_distinct(Vars), % every value must appear exactly once

    % Compute magic sum
    Sum is N * (N * N + 1) // 2,

    % Enforce row sums
    maplist(row_sum(Sum), Square), % each row must add up to Sum: maplist calls row_sum for every row in Square

    % Enforce column sums
    transpose(Square, Columns), % first transpose the matrix
    maplist(row_sum(Sum), Columns), % now check column sums like row sums

    % Enforce main diagonal sums
    Last is N - 1,
    numlist(0, Last, Indices), % creates a list [0,1,2,...,N-1]
    maplist(diag1_cell(Square), Indices, Diag1),  % collect the main-diagonal cell for every index into Diag1
    row_sum(Sum, Diag1), % diagonal must add up to Sum

    % Enforce anti-diagonal
    maplist(diag2_cell(Square, N), Indices, Diag2), % collect the anti-diagonal cell for every index into Diag2
    row_sum(Sum, Diag2), % diagonal must add up to Sum

    % Search for concrete values satisfying all constraints
    label(Vars). % assign actual numbers to all variables

% ----------------
% HELPER FUNCTIONS
% Swap the parameters for the maplist function.
% length_(+N, ?List): List is a list of N elements
length_(N, List) :- length(List, N).

% Enforce that a list sums to Sum
% row_sum(+Sum, +Row)
row_sum(Sum, Row) :- sum(Row, #=, Sum).

% gets cells with index (I,I)
% nth0 gives us the element at position I
% diag1_cell(+Square, +I, -Cell)
diag1_cell(Square, I, Cell) :-
    nth0(I, Square, Row), % get row number I
    nth0(I, Row, Cell). % get element number I from that row

% Get cell (I, N-1-I) for anti-diagonal
% diag2_cell(+Square, +N, +I, -Cell)
diag2_cell(Square, N, I, Cell) :-
    nth0(I, Square, Row), % get row number I
    J is N - 1 - I,  % column index
    nth0(J, Row, Cell). % get element number J from that row
