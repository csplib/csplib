/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            SICStus Prolog                     */
/*                                                                         */
/* Name           : magicsq.pl                                             */
/* Title          : magic squares                                          */
/* Author         : Mats Carlsson                                          */
/* Date           : January 2002                                           */
/*                                                                         */
/* In a magic square, the elements are all different, and the sum of each  */
/* column, each row, and main diagonal, are all the same.                  */
/*                                                                         */
/* | ?- magic_square([ff,bisect], 7).                                      */
/*                                                                         */
/*-------------------------------------------------------------------------*/

:- module(magicsq, [magic_square/2]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

magic_square(Lab, N) :-
	problem(N, Vars),
	labeling(Lab, Vars),
	format('Magic ~d x ~d square:\n', [N,N]),
	fmt(N, Fmt, []),
	(   fromto(Vars,S0,S,[]),
	    param([N,Fmt])
	do  (   for(_,1,N),
		fromto(S0,[X|S1],S1,S),
		fromto(Row,[X|R],R,[])
	    do  true
	    ),
	    format(Fmt, Row)
	).

fmt(0) --> !, "\n".
fmt(I) --> "~t~d~+",
	{J is I-1},
	fmt(J).


problem(N, Square) :-
	NN is N*N,
	length(Square0, NN),
	domain(Square0, 1, NN),
	sort(Square0, Square),
	all_different(Square, [consistency(domain)]),
	Sum is (N*(NN+1))>>1,
	/* essential constraints */
	rows(0, N, Square, Ss, Ss1),
	columns(0, N, Square, Ss1, [SO,SW]),
	Nup is N+1,
	elts(N, 1, Nup, Square, SO),
	Ndown is N-1,
	elts(N, N, Ndown, Square, SW),
	(   foreach(S,Ss),
	    param(Sum)
	do  (   foreach(_,S),
		foreach(1,One)
	    do  true
	    ),
	    scalar_product(One, S, #=, Sum, [consistency(domain)])
	),
	/* symmetry breaking constraints */
	nth1(1, Square, X11),
	NNdown is NN-Ndown,
	nth1(NNdown, Square, XN1),
	nth1(N, Square, X1N),
	X11 #> X1N,
	X1N #> XN1,
	true.

rows(N, N, _) --> !.
rows(I, N, L) --> [Row],
	{J is I+1,
	 Start is I*N+1,
	 elts(N, Start, 1, L, Row)},
	rows(J, N, L).

columns(N, N, _) --> !.
columns(I, N, L) --> [Column],
	{J is I+1,
	 elts(N, J, N, L, Column)},
	columns(J, N, L).

elts(0, _, _, _, []) :-!.
elts(J, Index, Step, L, [X|Xs]) :-
	nth1(Index, L, X),
	I is J-1,
	Jndex is Index+Step,
	elts(I, Jndex, Step, L, Xs).
