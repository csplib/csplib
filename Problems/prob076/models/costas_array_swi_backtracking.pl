:- use_module(library(lists)).

% costas(+N, -Xs): builds a Costas Array of size N
costas(N, Xs) :- % entry point: builds a Costas Array of size N
    numlist(1, N, Domain), % Domain = [1,2,...,N], the values to place
    N1 is N - 1, % there are N-1 difference levels
    length(Buckets, N1), % one bucket per level
    maplist(=([]), Buckets), % every level starts with no diffs used
    build(Domain, [], Buckets, Xs). % start building with empty placement and empty buckets

% Avail: values not yet placed
% Placed: values placed so far, in reverse order (most recent first)
% Buckets: list of N-1 buckets, bucket L holds diffs already used at level L
% Xs: final result, only bound in the base case

% base case: all values placed
% build(+Avail, +Placed, +Buckets, -Xs): place the remaining Avail values one
% by one, recording level diffs in Buckets, until Xs is the finished array
build([], Placed, _, Xs) :- % all values placed, construct result
    reverse(Placed, Xs). % Placed was built in reverse, flip it to get Xs
build(Avail, Placed, Buckets, Xs) :- % recursive case: still values to place
    select(Can, Avail, Rest), % pick candidate Can from Avail, Rest = Avail without Can
    check_diffs(Can, Placed, Buckets, NewBuckets), % verify Can and record its new diffs
    build(Rest, [Can|Placed], NewBuckets, Xs). % recurse with Can placed and updated buckets

% check_diffs(+Can, +Placed, +Buckets, -NewBuckets)
% Walk Placed and Buckets in lockstep: the H at position L-1 in Placed pairs with bucket L.
% For each H, check that Can-H is unused at that level, then add it to that bucket.
check_diffs(_, [], Buckets, Buckets). % base case: no prior values left to check
check_diffs(Can, [H|T], [Bucket|Bs], [[D|Bucket]|NBs]) :- % check Can against H at this level
    D is Can - H, % compute difference: new value minus value placed L steps ago
    \+ memberchk(D, Bucket), % reject if this diff already exists at this level
    check_diffs(Can, T, Bs, NBs). % recurse on the next level's bucket
