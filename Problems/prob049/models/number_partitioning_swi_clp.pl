:- use_module(library(clpfd)).

% Number Partitioning (CSPLib prob049)
% Split the numbers 1..N into two sets A and B of equal size such that
% sum(A) = sum(B) and the sum of squares of A equals that of B.

% partition(+N, -A, -B)
partition(N, A, B) :-
    0 is N mod 2, % equal-sized sets require an even N
    Total is N * (N + 1) // 2, % sum of 1..N
    0 is Total mod 2, % the sum must be splittable in half
    HalfSum is Total // 2,
    TotalSq is N * (N + 1) * (2*N + 1) // 6, % sum of squares of 1..N
    0 is TotalSq mod 2, % the sum of squares too
    HalfSq is TotalSq // 2,
    Half is N // 2, % target size of each set
    numlist(1, N, Nums), % Nums = [1,2,...,N]
    length(Sels, N), % one indicator variable per number
    Sels ins 0..1, % Sel_i = 1 -> number i in set A, 0 -> set B
    Sels = [1|_], % symmetry break: number 1 always in set A
    sum(Sels, #=, Half), % set A (and so set B) holds N/2 elements
    scalar_product(Nums, Sels, #=, HalfSum), % sum(A) = Total/2
    maplist(square, Nums, Squares), % Squares = [1,4,9,...,N*N]
    scalar_product(Squares, Sels, #=, HalfSq), % sum of squares of A = TotalSq/2
    label(Sels),
    split(Nums, Sels, A, B). % read the two sets off the indicators

% square(+X, -Sq): Sq is X squared
square(X, Sq) :- Sq is X * X.

% split(+Nums, +Sels, -A, -B): sort each number into A or B by its indicator
split([], [], [], []).
split([X|Xs], [1|Ss], [X|A], B) :- split(Xs, Ss, A, B).
split([X|Xs], [0|Ss], A, [X|B]) :- split(Xs, Ss, A, B).
