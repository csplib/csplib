/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Langford's Number Problem
 * Author    : Mats Carlsson
 *
 * | ?- steiner(dual,[],33,bound).
 */

:- module(steiner, [steiner/4]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

steiner(dual, _Opt, N, Consistency) :-
	problem(N, Triples, Consistency),
	append(Triples, Vars),
	dual_labeling(0, N, Vars),
	format_steiner(Triples, N).
steiner(byrow, Opt, N, Consistency) :-
	problem(N, Triples, Consistency),
	append(Triples, Vars),
	labeling(Opt, Vars),
	format_steiner(Triples, N).
steiner(bycol, Opt, N, Consistency) :-
	problem(N, Triples, Consistency),
	transpose(Triples, Transpose),
	append(Transpose, Vars),
	labeling(Opt, Vars),
	format_steiner(Triples, N).


format_steiner(Triples, N) :-
	format('Steiner instance of order ~d:\n', [N]),
	(   foreach(T,Triples)
	do  format('~t~d~+~t~d~+~t~d~+\n', T)
	).

dual_labeling(N, N, _) :- !.
dual_labeling(N1, N3, Vars) :-
	N2 is N1+1,
	split_by_min(Vars, N2, Cands, Rest, Rest2),
	M is N3>>1,
	dual_choose(0, M, N2, Cands, Rest2),
	dual_labeling(N2, N3, Rest).

split_by_min([], _, []) --> [].
split_by_min([X|L1], N, L2) --> [X],
	{fd_min(X, Xmin)},
	{Xmin=\=N}, !,
	split_by_min(L1, N, L2).
split_by_min([X|L1], N, [X|L2]) -->
	split_by_min(L1, N, L2).
	
dual_choose(M, M, _, Cands, Cands) :- !.
dual_choose(I, M, Val, [Val|Cands], Rest) :-
	J is I+1,
	dual_choose(J, M, Val, Cands, Rest).
dual_choose(I, M, Val, [X|Cands], [X|Rest]) :-
	X #\= Val,
	dual_choose(I, M, Val, Cands, Rest).

problem(N, Triples, Consistency) :-
	M is N mod 6,
	(M=:=1 ; M=:=3), !,
	NTrip is N*(N-1)//6,
	length(Triples, NTrip),
	(   foreach([A,B,C],Triples),
	    foreach([A,B],Tuples),
	    param(N)
	do  domain([A,B,C], 1, N),
	    A #< B, B #< C
	),
	lex_chain(Tuples,[increasing]),
	pair_constraints(Triples, N, Consistency),
	card_constraint(Triples, N).
	
pair_constraints(Triples, N, Consistency) :-
	(   foreach([A,B,C],Triples),
	    fromto(Codes,[AB,AC,BC|S],S,[]),
	    param([N,Consistency])
	do  scalar_product([N,1], [A,B], #=, AB, [consistency(Consistency)]),
	    scalar_product([N,1], [A,C], #=, AC, [consistency(Consistency)]),
	    scalar_product([N,1], [B,C], #=, BC, [consistency(Consistency)])
	),
	all_distinct(Codes, [consistency(Consistency)]).

card_constraint(Triples, N) :-
	M is N>>1,
	(   for(J,1,N),
	    foreach(J-M,Cs),
	    param(M)
	do  true
	),
	append(Triples, Vars),
	global_cardinality(Vars, Cs).
