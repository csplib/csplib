:- use_module(library(lists)).

% Number Partitioning (CSPLib prob049)
% Split the numbers 1..N into two sets A and B of equal size such that
% sum(A) = sum(B) and the sum of squares of A equals that of B.

% partition(+N, -A, -B)
partition(N, A, B) :-
    0 is N mod 2,                       % equal-sized sets require an even N
    Total is N * (N + 1) // 2,          % sum of 1..N
    0 is Total mod 2,                   % the sum must be splittable in half
    HalfSum is Total // 2,
    TotalSq is N * (N + 1) * (2*N + 1) // 6,   % sum of squares of 1..N
    0 is TotalSq mod 2,                 % the sum of squares too
    HalfSq is TotalSq // 2,
    Half is N // 2,                     % target size of each set
    N1 is N - 1,
    numlist(1, N1, Pool),               % candidates still to assign: 1..N-1
    NSq is N * N,
    % symmetry break: the largest number N is placed in set A up front
    place(Pool, HalfSum, HalfSq, Half, [N], N, NSq, 1, A0),
    sort(A0, A),                        % set A in ascending order
    numlist(1, N, All),
    subtract(All, A, B).                % set B is everything not in A

% place(+Remaining, +HalfSum, +HalfSq, +Half, +AccA, +SumA, +SqA, +SizeA, -A)
% Decide set membership for each remaining number. Only set A is tracked:
% once A has hit every target, B is forced to match (the grand totals are fixed).

% base case: no numbers left -- A must have hit all three targets exactly
place([], HalfSum, HalfSq, Half, AccA, HalfSum, HalfSq, Half, AccA).
% option 1: put X into set A
place([X|Xs], HalfSum, HalfSq, Half, AccA, SumA, SqA, SizeA, A) :-
    SumA1 is SumA + X,    SumA1  =< HalfSum,    % must not overshoot the sum
    SizeA1 is SizeA + 1,  SizeA1 =< Half,       % must not overshoot the size
    SqA1 is SqA + X * X,  SqA1   =< HalfSq,     % must not overshoot the squares
    place(Xs, HalfSum, HalfSq, Half, [X|AccA], SumA1, SqA1, SizeA1, A).
% option 2: put X into set B (simply leave it out of A)
place([_|Xs], HalfSum, HalfSq, Half, AccA, SumA, SqA, SizeA, A) :-
    place(Xs, HalfSum, HalfSq, Half, AccA, SumA, SqA, SizeA, A).
