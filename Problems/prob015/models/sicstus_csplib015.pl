/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Schur's Lemma
 * Author    : Mats Carlsson
 *
 * | ?- schur(122,5).
 */

:- module(schur, [schur/2]).

:- use_module(library(trees)).
:- use_module(library(clpfd)).

schur(N, P) :-
	length(Integers, N),
	length(Binaries, N),
	(   foreach(F,Binaries),
	    param(P)
	do  functor(F, f, P)
	),
	list_to_tree(Binaries, Tree),
	domain(Integers, 1, P),
	(   for(K,1,P),
	    fromto(Relation,[[K|Row]|S],S,[]),
	    param(P)
	do  (   for(J,1,P),
		fromto(Row,[ZO|S1],S1,[]),
		param(K)
	    do  (J=:=K -> ZO=1 ; ZO=0)
	    )
	),
	(   foreach(Int,Integers),
	    foreach(Bin,Binaries),
	    foreach([Int|ZOs],Table)
	do  Bin =.. [_|ZOs],
	    domain(ZOs, 0, 1)
	),
	table(Table, Relation),
	(   for(I,0,(N>>1)-1),
	    param([N,Tree])
	do  I1 is I+1,
	    get_label(I1, Tree, IL),
	    (   for(J0,I,N-I1-1),
		param([I1,IL,Tree])
	    do  J1 is J0+1,
		get_label(J1, Tree, JL),
		Key is J1+I1,
		get_label(Key, Tree, KL),
		(   foreacharg(IA,IL),
		    foreacharg(JA,JL),
		    foreacharg(KA,KL)
		do  at_most_two(IA, JA, KA)
		)
	    )
	),
	symmetry_labeling(Integers, 1, P),
	writeq(Integers),
	nl.

at_most_two(IA, JA, KA) +:
	IA + JA + KA #=< 2.

symmetry_labeling(Vars, K, K) :- !,
	labeling([enum], Vars).
symmetry_labeling([], _, _).
symmetry_labeling([V|Vars], Lim, K) :-
	V #=< Lim,
	indomain(V),
	(V<Lim -> Lim1=Lim ; Lim1 is Lim+1),
	symmetry_labeling(Vars, Lim1, K).
