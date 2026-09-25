:- use_module(library(lists)). % numlist/3, select/3, memberchk/2

% All-Interval Series (CSPLib prob007)
% Find a permutation S of {0..N-1} such that the consecutive absolute
% differences also form a permutation of {1..N-1}.
%
% Example: ?- all_interval(8, S).

% all_interval(+N, -S)
all_interval(N, [H|T]) :-
    length([H|T], N),
    N1 is N - 1,
    numlist(0, N1, Domain), % Domain = [0,1,...,N-1]
    select(H, Domain, Rest), % first element, no interval yet
    build(T, Rest, [], H).

% build(-S, +Avail, +Used, +Prev): fill S from Avail so that every
% interval to the previous element is new; Used holds the intervals so far.
build([], _, _, _).
build([H|T], Avail, Used, Prev) :-
    select(H, Avail, Rest),
    D is abs(H - Prev),
    \+ memberchk(D, Used), % interval not used yet
    build(T, Rest, [D|Used], H).
