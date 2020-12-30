/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Langford's Number Problem
 * Author    : Mats Carlsson
 *
 * | ?- langford(3,10).
 */

:- module(langford, [langford/2]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

langford(K, N) :-
	NK is N*K,
	length(Pos, NK),
	domain(Pos, 1, NK),	% Pos[ns]: position of (number, set)
				% pair in the sought sequence
	(   for(I,1,N),
	    param(K,Pos)
	do  (   for(J,1,K-1),
		param(I,K,Pos)
	    do  Ix1 is K*(I-1) + J+1,
		Ix2 is Ix1-1,
		I1 is I+1,
		nth1(Ix1, Pos, Pos1),
		nth1(Ix2, Pos, Pos2),
		Pos1 - Pos2 #= I1
	    )
	),
	length(Num, NK),
	domain(Num, 1, NK),	% Num[p]: (number, set) pair at
				% position p in the sought sequence
	assignment(Pos, Num),
	labeling([min,bisect], Num), % by trial and error
	format('~w\n~w\n', [Pos,Num]),
	(   for(I3,1,N),
	    fromto(Pos,Posa,Posb,[]),
	    param(K)
	do  length(Prefix, K),
	    append(Prefix, Posb, Posa),
	    format('Position of "~d": ~w\n', [I3,Prefix])
	).
