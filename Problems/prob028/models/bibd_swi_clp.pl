:- use_module(library(clpfd)).

% Balanced Incomplete Block Design (CSPLib prob028)
% Build the V x B incidence matrix of a (V, B, R, K, L) design:
%   - every row sums to R     (each object appears in R blocks)
%   - every column sums to K  (each block contains K objects)
%   - the scalar product of any two rows is L
%     (each pair of objects occurs together in exactly L blocks)

% bibd(+V, +B, +R, +K, +L, -Rows)
bibd(V, B, R, K, L, Rows) :-
    length(Rows, V),                    % one row per object
    maplist(new_row(B), Rows),          % each row is B many 0/1 variables
    maplist(line_sum(R), Rows),         % each object appears in R blocks
    transpose(Rows, Cols),
    maplist(line_sum(K), Cols),         % each block contains K objects
    lex_chain(Rows),                    % symmetry break: rows ordered
    lex_chain(Cols),                    % symmetry break: columns ordered
    pairwise_overlap(Rows, L),          % every pair of rows overlaps in L blocks
    append(Rows, Cells),
    labeling([ff], Cells).

% new_row(+B, -Row): a row of B variables, each 0 or 1
new_row(B, Row) :-
    length(Row, B),
    Row ins 0..1.

% line_sum(+S, +Line): the entries of Line add up to S
line_sum(S, Line) :- sum(Line, #=, S).

% pairwise_overlap(+Rows, +L): scalar product of every pair of rows equals L
pairwise_overlap([], _).
pairwise_overlap([Row|Rest], L) :-
    maplist(overlap(Row, L), Rest),
    pairwise_overlap(Rest, L).

% overlap(+Row1, +L, +Row2): the two rows share exactly L set bits
overlap(Row1, L, Row2) :-
    maplist(prod, Row1, Row2, Prods),   % Prods[i] = Row1[i] * Row2[i]
    sum(Prods, #=, L).

% prod(?X, ?Y, ?P): P = X*Y, the logical AND of 0/1 entries (1 only when both are 1)
prod(X, Y, P) :- P #= X * Y.
