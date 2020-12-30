/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Golomb Ruler
 * Author    : Mats Carlsson
 *
 * | ?- golomb([], 12, bound).
 */

:- module(golomb, [
	golomb/3
	]).
:- use_module(library(lists), [
	last/2
	]).
:- use_module(library(clpfd)).

%
% compute an optimum golomb ruler with N marks
%
golomb(Lab, N, Consistency) :-
	marks(N, Marks, Last, [consistency(Consistency)]),
	indomain(Last),
	labeling(Lab, Marks),
	writeq(Marks),
	nl.

marks(N1, [0|Xs], Xn, Opt) :-
	N is N1-1,
	length(Xs, N),
	Max is N*N,
	domain(Xs, 1, Max),
	deltas(Xs, Triangle, Opt, Ds, Xs),
	(   foreach(Row,[Xs|Triangle])
	do  (   foreach(D,Row),
		count(J,1,_)
	    do  d(J, N2),
		D #>= N2
	    )
	),
	Xs = [X1|_],
	last(Xs, Xn),
	last(Triangle, [Dmn]),
	X1 #< Dmn,		% break symmetries
	all_distinct(Ds, Opt).

deltas([_], [], _Opt) --> !.
deltas([X|Xs], [Row|Triangle], Opt) -->
	(   foreach(Xj,Xs),
	    foreach(Dij,Row),
	    param([X,Opt])
	do  [Dij],
	    {scalar_product([1,-1], [Xj,X], #=, Dij, Opt)}
	),
	deltas(Xs, Triangle, Opt).

d( 1,  1).
d( 2,  3).
d( 3,  6).
d( 4,  11).
d( 5,  17).
d( 6,  25).
d( 7,  34).
d( 8,  44).
d( 9,  55).
d(10,  72).
d(11,  85).
d(12, 106).
d(13, 127).
d(14, 151).
d(15, 177).
d(16, 199).
d(17, 216).
d(18, 246).
d(19, 283).
d(20, 333).
d(21, 356).
d(22, 372).
d(23, 425).

