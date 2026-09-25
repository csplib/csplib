:- use_module(library(clpfd)).

% Langford's Number Problem (CSPLib prob024)
% Place two copies of each number 1..N in a 2N sequence such that
% the two k's are exactly k positions apart.
% Only solvable when N mod 4 = 0 or N mod 4 = 3.

% predefined partial sequence (use _ for unknowns)
puzzle(4, [_, 1, _, 1, _, _, _, _]).

% solve the predefined puzzle
% solve(-S)
solve(S) :-
    puzzle(N, S), % unifies N with the length and S with the predefined sequence
    langford(N, S).

% langford(+N, ?S)
langford(N, S) :-
    Len is 2 * N,  % sequence has 2N slots
    length(S, Len), % S = [_,_,...,_] with Len placeholders
    S ins 1..N, % every value in S must be between 1 and N
    numlist(1, N, Ks), % Ks = [1, 2, ..., N]
    constrain_all(Ks, S, Len, Positions),
    all_distinct(Positions),
    label(Positions).

% constrain_all(+Ks, ?S, +Len, -Positions)
% base case: no numbers left to place
constrain_all([], _, _, []).
constrain_all([K|Ks], S, Len, [P1,P2|Ps]) :-
    P1 in 1..Len, % first position of K anywhere in the sequence
    P2 #= P1 + K + 1, % second position is exactly K+1 slots after the first
    P2 #=< Len,  % second position must not exceed the sequence length
    element(P1, S, K), % S[P1] = K
    element(P2, S, K), % S[P2] = K
    constrain_all(Ks, S, Len, Ps).
