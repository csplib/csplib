% A magic sequence of length n is a sequence x0..x(n-1) where each value
% is between 0 and n-1, and xi equals the number of times i occurs in the sequence.
% Example (n=10): [6,2,1,0,0,0,1,0,0,0]

% magic_sequence(+N, -Seq)
magic_sequence(N, Seq) :-
    length(Seq, N),
    N1 is N - 1,
    numlist(0, N1, Domain),          % each value must be in 0..N-1
    fill(Seq, Domain, Seq).

% fill(+Cells, +Domain, +Seq)
fill([], _, _).
fill([H|Rest], Domain, Seq) :-
    member(H, Domain),               % assign value from 0..N-1 (values can repeat, so no select)
    check_partial(Seq),              % check after every assignment
    fill(Rest, Domain, Seq).


% Incomplete: for each filled position i with value xi,
% the current count of i in filled cells must not exceed xi.
% check_partial(+Seq)
check_partial(Seq) :-
    include(nonvar, Seq, Filled),
    length(Seq, N),
    length(Filled, M), M < N,        % sequence not yet complete
    check_counts(Seq, Filled, 0).

% Complete: count of i must equal xi exactly for every position.
check_partial(Seq) :-
    include(nonvar, Seq, Seq),       % all cells filled
    verify_all(Seq, Seq, 0).


% For each filled position i: count of i in filled cells must not exceed xi.
% check_counts(+Cells, +Filled, +I)
check_counts([], _, _).
check_counts([Cell|Rest], Filled, I) :-
    nonvar(Cell),
    count_occurrences(I, Filled, Count),
    Count =< Cell,                   % partial count must not exceed assigned value
    NextI is I + 1,
    check_counts(Rest, Filled, NextI).
check_counts([Cell|Rest], Filled, I) :-
    var(Cell),                       % position not yet assigned, skip
    NextI is I + 1,
    check_counts(Rest, Filled, NextI).


% Verifies complete sequence: count of i must equal xi for every position.
% verify_all(+Cells, +Seq, +I)
verify_all([], _, _).
verify_all([Cell|Rest], Seq, I) :-
    count_occurrences(I, Seq, Count),
    Count =:= Cell,
    NextI is I + 1,
    verify_all(Rest, Seq, NextI).


% count_occurrences(+X, +List, -Count): Count is how often X occurs in List
count_occurrences(X, List, Count) :-
    include(=(X), List, Matches),   % keep only elements equal to X
    length(Matches, Count).
