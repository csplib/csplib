:- use_module(library(lists)). % numlist/3, select/3, member/2

% All-Interval Series (CSPLib prob007)
% Find a permutation S of {0..N-1} such that the consecutive absolute
% differences also form a permutation of {1..N-1}.

% all_interval(+N, -S)
all_interval(N, S) :-
    length(S, N), % S = [_,_,_,...,_] with N placeholders
    N1 is N - 1,
    numlist(0, N1, Domain), % Domain= [0,1,2,...,N-1]
    build(S, Domain, [], none).

% build(-S, +Avail, +Used, +Prev): fill S choosing from Avail, Prev is the previous element or none
build([], _, _, _).
% for the first element (prev= none)
build([H|T], Avail, Used, none) :-
    select(H, Avail, Rest),
    build(T, Rest, Used, H).
build([H|T], Avail, Used, Prev) :-
    Prev \= none,
    select(H, Avail, Rest),
    D is abs(H - Prev),
    \+ member(D, Used),  % checks if distance is not used
    build(T, Rest, [D|Used], H). % Adds D to Used distances
