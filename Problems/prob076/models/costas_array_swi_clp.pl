:- use_module(library(clpfd)).

% Costas Array (CSPLib prob076)
% A permutation Xs of 1..N where all differences within each level of the difference triangle are distinct.
% Level L contains differences X(i+L) - X(i) for all valid i.

% costas(?Xs): user provides a list, N is inferred from its length
costas(Xs) :- % entry point: user provides a list, N is inferred from its length
    length(Xs, N),
    costas(N, Xs).

% costas(+N, ?Xs): Xs is a Costas Array of size N
costas(N, Xs) :-
    length(Xs, N), % Xs= [_,_,_, ... ,_] with N variables
    Xs ins 1..N, % each value is a row position 1..N
    all_distinct(Xs), % one mark per row (permutation)
    N1 is N - 1,
    numlist(1, N1, Levels), % Levels = [1, 2, 3, ..., N-1] of the difference triangle
    maplist(level_all_distinct(Xs), Levels), % each level must have distinct differences
    label(Xs).

% level_all_distinct(+Xs, +L): differences at level L must all be distinct
level_all_distinct(Xs, L) :-
    level_diffs(Xs, L, Diffs), % compute differences X(i+L) - X(i) for all valid i
    all_distinct(Diffs).

% level_diffs(+Xs, +L, -Diffs): collect differences at level L
level_diffs(Xs, Lev, Diffs) :-
    length(Drop, Lev), % Drop = [_,_, .. ,_ ] with Length = Lev
    append(Drop, Shifted, Xs), % Shifted = elements from position L+1 onwards in Xs
    length(Shifted, LenDiff), % LenDiff= How many differences there will be
    length(Base, LenDiff), % Base = [_,_, ... ,_] with length of LenDiff
    append(Base, _, Xs), % Base = first LenDiff elements of Xs
    maplist(diff, Base, Shifted, Diffs). % D = X(i+L) - X(i)

% diff(+A, +B, -D): D is the difference B - A
diff(A, B, D) :- D #= B - A.
