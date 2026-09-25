:- use_module(library(clpfd)).

% All-Interval Series (CSPLib prob007)
% Find a permutation S of {0..N-1} such that the consecutive absolute
% differences also form a permutation of {1..N-1}.
%
% Example: ?- all_interval(8, S).

% all_interval(+N, -S)
all_interval(N, S) :-
    length(S, N),
    N1 is N - 1,
    S ins 0..N1,
    all_distinct(S),
    consecutive_diffs(S, Diffs),
    Diffs ins 1..N1,
    all_distinct(Diffs),
    label(S).

% consecutive_diffs(+Series, -Diffs)
% Diffs[i] = |S[i+1] - S[i]| for each consecutive pair.
consecutive_diffs([_], []).
consecutive_diffs([A,B|Rest], [D|Ds]) :-
    D #= abs(B - A), % D = |B - A|, posted as a constraint (A, B may be unbound)
    consecutive_diffs([B|Rest], Ds).
