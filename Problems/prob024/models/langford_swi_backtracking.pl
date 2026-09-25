% Langford's Number Problem (CSPLib prob024)
% Place two copies of each number 1..N in a 2N sequence such that
% the two k's are exactly k positions apart.
% Only solvable when N mod 4 = 0 or N mod 4 = 3.

% predefined partial sequence (use _ for unknowns)
puzzle(4, [_, 1, _, 1, _, _, _, _]).

% solve the predefined puzzle
% solve(-S)
solve(S) :-
    puzzle(N, S),                    % unifies N with the length and S with the predefined sequence
    langford(N, S).

% langford(+N, ?S)
langford(N, S) :-
    Len is 2 * N, % sequence has 2N slots (two copies of each number)
    length(S, Len), % S = [_,_,...,_] with Len placeholders
    numlist(1, N, Ks),  % Ks = [1, 2, ..., N]
    reverse(Ks, KsRev),  % reverse to [N, ..., 2, 1]: place larger numbers first to be more efficient
    place_all(KsRev, S, Len). % place each number into S

% place_all(+Ks, ?S, +Len)
% base case: all numbers placed
place_all([], _, _).
place_all([K|Ks], S, Len) :-
    place(K, S, Len),  % place K at two valid positions
    place_all(Ks, S, Len).

% place(+K, ?S, +Len)
% try positions I and J = I+K+1, assign K to both slots
place(K, S, Len) :-
    between(1, Len, I), % try each possible start position (backtracking)
    J is I + K + 1, % second position is K+1 slots after the first
    J =< Len, % second position must not exceed sequence length
    nth1(I, S, K), % S[I] = K (fails if slot is already taken)
    nth1(J, S, K). % S[J] = K (fails if slot is already taken)
