:- use_module(library(lists)).

% Balanced Incomplete Block Design (CSPLib prob028)
% Build the V x B incidence matrix of a (V, B, R, K, L) design one row
% at a time. Each row has exactly R ones; each column must end on exactly
% K ones; every pair of rows must share exactly L ones.

% bibd(+V, +B, +R, +K, +L, -Rows)
bibd(V, B, R, K, L, Rows) :-
    zeros(B, ColSums),                  % running column totals, all 0 to start
    build(V, B, R, K, L, ColSums, [], Rows).

% build(+RowsLeft, +B, +R, +K, +L, +ColSums, +Placed, -Rows)
% base case: no rows left -- columns and overlaps must hit their targets
build(0, _, _, K, L, ColSums, Placed, Rows) :-
    maplist(=(K), ColSums),             % every column total must equal K
    reverse(Placed, Rows),
    all_pairs(Rows, L).                 % every pair of rows must share L ones
build(N, B, R, K, L, ColSums, Placed, Rows) :-
    N > 0,
    row(B, R, Row),                     % a fresh row: B entries, exactly R ones
    add_cols(Row, ColSums, ColSums1),   % fold the row into the column totals
    N1 is N - 1,
    cols_ok(ColSums1, K, N1),           % no column over K, all still reachable
    overlaps_ok(Row, Placed, L),        % shares at most L ones with each placed row
    build(N1, B, R, K, L, ColSums1, [Row|Placed], Rows).

% zeros(+N, -List): a list of N zeros
zeros(0, []).
zeros(N, [0|Zs]) :- N > 0, N1 is N - 1, zeros(N1, Zs).

% row(+Len, +Ones, -Row): a 0/1 list of length Len holding exactly Ones ones
row(0, 0, []).
row(Len, Ones, [1|T]) :-
    Len > 0, Ones > 0,
    Len1 is Len - 1, Ones1 is Ones - 1,
    row(Len1, Ones1, T).
row(Len, Ones, [0|T]) :-
    Len > 0, Len > Ones,                % keep enough room for the remaining ones
    Len1 is Len - 1,
    row(Len1, Ones, T).

% add_cols(+Row, +ColSums, -ColSums1): add the row element-wise to the totals
add_cols([], [], []).
add_cols([X|Xs], [C|Cs], [C1|C1s]) :-
    C1 is C + X,
    add_cols(Xs, Cs, C1s).

% cols_ok(+ColSums, +K, +RowsLeft): no column exceeds K and each can still
% reach K with the rows that remain
cols_ok([], _, _).
cols_ok([C|Cs], K, RowsLeft) :-
    C =< K,
    C + RowsLeft >= K,
    cols_ok(Cs, K, RowsLeft).

% overlaps_ok(+Row, +Placed, +L): Row shares at most L ones with each placed row
overlaps_ok(_, [], _).
overlaps_ok(Row, [P|Ps], L) :-
    dot(Row, P, D),
    D =< L,
    overlaps_ok(Row, Ps, L).

% all_pairs(+Rows, +L): the scalar product of every pair of rows equals L
all_pairs([], _).
all_pairs([Row|Rest], L) :-
    pair_with(Row, Rest, L),
    all_pairs(Rest, L).

% pair_with(+Row, +Rows, +L): the scalar product of Row with each row in Rows equals L
pair_with(_, [], _).
pair_with(Row, [P|Ps], L) :-
    dot(Row, P, L),
    pair_with(Row, Ps, L).

% dot(+Xs, +Ys, ?D): D is the scalar product of two equal-length lists
dot([], [], 0).
dot([X|Xs], [Y|Ys], D) :-
    dot(Xs, Ys, D0),
    D is D0 + X * Y.
