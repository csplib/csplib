/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Word Design for DNA Computing on Surfaces
 * Author    : Mats Carlsson
 *
 * | ?- dna(plain,86).
 */

:- module(dna, [dna/2]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

dna(greedy, NbOctos) :-
	greedy(0, NbOctos, [], Octos),
	display_each(Octos).
dna(plain, NbOctos) :-
	system(NbOctos, Octos),
	append(Octos, Vars),
	labeling([], Vars),
	display_each(Octos).
dna(lds(N), NbOctos) :-
	system(NbOctos, Octos),
	append(Octos, Vars),
	labeling([discrepancy(N)], Vars),
	display_each(Octos).
dna(sandwich, NbOctos) :-
	system(NbOctos, Octos),
	reverse(Octos, OctosR),
	(   foreach(O,Octos),
	    foreach(R,OctosR)
	do  labeling([], O),
	    labeling([down], R)
	),
	display_each(Octos).
dna(min_conflict, NbOctos) :-
	system(NbOctos, Octos),
	lab_min_conflict(Octos, []),
	display_each(Octos).
dna(each, NbOctos) :-
	system(NbOctos, Octos),
	(   foreach(Octo,Octos)
	do  labeling([], Octo)
	),
	display_each(Octos).
dna(shave, NbOctos) :-
	system(NbOctos, Octos),
	(   foreach(Octo,Octos)
	do  (   (   foreach(X,Octo)
		do  findall(X, indomain(X), Feas),
		    list_to_fdset(Feas, Set),
		    X in_set Set
		),
		labeling([], Octo) -> true
	    )
	),
	display_each(Octos).
dna(seed, NbOctos) :-
	system(NbOctos, Octos),
	append(Octos, Vars),
	(   foreach(Octo,Octos),
	    count(I,0,_),
	    param(NbOctos)
	do  Octo = [A|_],
	    Bin is (I<<3)//NbOctos,
	    seed(Bin, A)
	),
	labeling([], Vars),
	display_each(Octos).

lab_min_conflict([], _).
lab_min_conflict([Octo|Octos], Done) :-
	Octo = [A,B,C,D,E,F,G,H],
	labeling([], [A,B,C,D,E]),
	lab_min_conflict(F, 6, Done),
	lab_min_conflict(G, 7, Done),
	lab_min_conflict(H, 8, Done),
	lab_min_conflict(Octos, [Octo|Done]).

lab_min_conflict(X, Pos, Done) :-
	var(X), !,
	conflicts(Done, Pos, S1, []),
	tally(S1, 0, K0, 0, K1, 0, K2, 0, K3),
	keysort([K0-0,K1-1,K2-2,K3-3], S2),
	member(_-X, S2).
lab_min_conflict(_, _, _).

conflicts([], _) --> [].
conflicts([D|Ds], Pos) --> [X,Y1],
	{conflict(Pos, D, X, Y)},
	{Y1 is 3-Y},
	conflicts(Ds, Pos).

conflict(6, [_,_,Y,_,_,X,_,_], X, Y).
conflict(7, [_,Y,_,_,_,_,X,_], X, Y).
conflict(8, [Y,_,_,_,_,_,_,X], X, Y).

tally([], K0, K0, K1, K1, K2, K2, K3, K3).
tally([X|Xs], K0, N0, K1, N1, K2, N2, K3, N3) :-
	tally(X, K0, M0, K1, M1, K2, M2, K3, M3),
	tally(Xs, M0, N0, M1, N1, M2, N2, M3, N3).

tally(0, K0, M0, K1, K1, K2, K2, K3, K3) :- M0 is K0+1.
tally(1, K0, K0, K1, M1, K2, K2, K3, K3) :- M1 is K1+1.
tally(2, K0, K0, K1, K1, K2, M2, K3, K3) :- M2 is K2+1.
tally(3, K0, K0, K1, K1, K2, K2, K3, M3) :- M3 is K3+1.

seed(0, 0).
seed(1, 0).
seed(2, 0).
seed(3, 1).
seed(4, 2).
seed(5, 3).
seed(6, 3).
seed(7, 3).

display_each([]).
display_each([Octo|Octos]) :-
	(   foreach(B,Octo),
	    foreach(C,String)
	do  acgt_map(B, C)
	),
	format('~s\n', [String]),
	display_each(Octos).

acgt_map(0, 0'A).
acgt_map(1, 0'T).
acgt_map(10, 0'C).
acgt_map(11, 0'G).

greedy(Nb, Nb, Octos, Octos) :- !.
greedy(Nb1, Nb3, Octos1, Octos3) :-
	Nb2 is Nb1+1,
	append(Octos1, [Octo], Octos2),
	system(Nb2, Octos2),
	labeling([], Octo), !,
	greedy(Nb2, Nb3, Octos2, Octos3).

system(NbOctos, Octos) :-
	length(Octos, NbOctos),
	(   foreach(O1,Octos)
	do  length(O1, 8),
	    (foreach(X,O1) do X in {0,1,10,11})
	),
	(   foreach(O2,Octos)
	do  Sum in 40..48,
	    sum(O2, #=, Sum)
	),
	(   foreach([A,B,C,D,E,_,_,_],Octos),
	    foreach([A,B,C,D,E],Pentas)
	do  true
	),
	lex_chain(Pentas, [op(#<)]), % saves runtime and backtracks
	all_compl_local(Octos).

/* Local handling of the all-diffs */

all_compl_local([]).
all_compl_local([O|Os]) :-
	reverse(O, R),
	all_differ4(Os, O),
	all_cdiffer4([O|Os], R),
	all_compl_local(Os).

all_differ4([], _) :- !.
all_differ4(Octos, RevOcto) :-
	extend_vars(Octos, RevOcto, L1, L2, L3),
	transpose([L1,L2,L3], Tuples),
	extension(eqs, Extension),
	table(Tuples, Extension).

all_cdiffer4(Octos, RevOcto) :-
	extend_vars(Octos, RevOcto, L1, L2, L3),
	transpose([L1,L2,L3], Tuples),
	extension(eqcs, Extension),
	table(Tuples, Extension).

extend_vars([], _, [], [], []).
extend_vars([O|Os], R, L1, L2, L3) :-
	length(D, 8),
	domain(D, 0, 1),
	sumle4(D),
	append(O, L1b, L1),
	append(R, L2b, L2),
	append(D, L3b, L3),
	extend_vars(Os, R, L1b, L2b, L3b).

% Z=1 iff X and Y are equal (eqs) or complementary (eqcs)
extension(eqs , [ [0,0,1],  [0,1,0],  [0,10,0],  [0,11,0],
		  [1,0,0],  [1,1,1],  [1,10,0],  [1,11,0],
		 [10,0,0], [10,1,0], [10,10,1], [10,11,0],
		 [11,0,0], [11,1,0], [11,10,0], [11,11,1]]).
extension(eqcs, [ [0,0,0],  [0,1,1],  [0,10,0],  [0,11,0],
		  [1,0,1],  [1,1,0],  [1,10,0],  [1,11,0],
		 [10,0,0], [10,1,0], [10,10,0], [10,11,1],
		 [11,0,0], [11,1,0], [11,10,1], [11,11,0]]).

/* Utilities */

sumeq4([B1,B2,B3,B4,B5,B6,B7,B8]) :-
% 	sum(Bs, #=<, 4).
	sumeq4_ix(B1,B2,B3,B4,B5,B6,B7,B8).

sumeq4_ix(B1,B2,B3,B4,B5,B6,B7,B8) +:
	B1+B2+B3+B4+B5+B6+B7+B8 #= 4.

sumle4([B1,B2,B3,B4,B5,B6,B7,B8]) :-
% 	sum(Bs, #=<, 4).
	sumle4_ix(B1,B2,B3,B4,B5,B6,B7,B8).

sumle4_ix(B1,B2,B3,B4,B5,B6,B7,B8) +:
	B1+B2+B3+B4+B5+B6+B7+B8 #=< 4.
%     B1 in !(0..(4        -min(B2)-min(B3)-min(B4)-min(B5)-min(B6)-min(B7)-min(B8))),
%     B2 in !(0..(4-min(B1)        -min(B3)-min(B4)-min(B5)-min(B6)-min(B7)-min(B8))),
%     B3 in !(0..(4-min(B1)-min(B2)        -min(B4)-min(B5)-min(B6)-min(B7)-min(B8))),
%     B4 in !(0..(4-min(B1)-min(B2)-min(B3)        -min(B5)-min(B6)-min(B7)-min(B8))),
%     B5 in !(0..(4-min(B1)-min(B2)-min(B3)-min(B4)        -min(B6)-min(B7)-min(B8))),
%     B6 in !(0..(4-min(B1)-min(B2)-min(B3)-min(B4)-min(B5)        -min(B7)-min(B8))),
%     B7 in !(0..(4-min(B1)-min(B2)-min(B3)-min(B4)-min(B5)-min(B6)        -min(B8))),
%     B8 in !(0..(4-min(B1)-min(B2)-min(B3)-min(B4)-min(B5)-min(B6)-min(B7)        )).

end_of_file.

/* Global handling by means of 56 all-diff constraints. */
/* Broken code based on a different ACGT encoding! */

all_compl_global(Octos) :-
	extension(eqcs, Extension),
	complements(Octos, Extension, Total, Octos),
	all_compl_global(0, 56, Total).

all_compl_global(N, N, _) :- !.
all_compl_global(I, N, Total) :-
	J is I+1,
	all_compl_codes(Total, J, Codes),
	all_distinct(Codes, [consistency(bound),on(minmax)]),
	all_compl_global(J, N, Total).

all_compl_codes([], _, []).
all_compl_codes([Octo|Octos], I, [Code|Codes]) :-
	choose(I, Octo, Penta),
	scalar_product([256,64,16,4,1], Penta, #=, Code),
	all_compl_codes(Octos, I, Codes).

complements([], _) --> [].
complements([[A,B,C,D,E,F,G,H]|Octos], Extension) --> [[H1,G1,F1,E1,D1,C1,B1,A1]],
	{table([[A,A1,1],
		[B,B1,1],
		[C,C1,1],
		[D,D1,1],
		[E,E1,1],
		[F,F1,1],
		[G,G1,1],
		[H,H1,1]], Extension)},
	complements(Octos, Extension).

gen_choose :-
	length(L, 8),
	findall(L-P, choose5(L,P), All),
	gen_choose(All, 0).

gen_choose([], _).
gen_choose([L-P|Rest], I) :-
	J is I+1,
	portray_clause(choose(J, L, P)),
	gen_choose(Rest, J).

choose5(L1, [A,B,C,D,E]) :-
	suffix(L1, [A|L2]),
	suffix(L2, [B|L3]),
	suffix(L3, [C|L4]),
	suffix(L4, [D|L5]),
	suffix(L5, [E|_]).

choose(1, [A,B,C,D,E,_,_,_], [A,B,C,D,E]).
choose(2, [A,B,C,D,_,E,_,_], [A,B,C,D,E]).
choose(3, [A,B,C,D,_,_,E,_], [A,B,C,D,E]).
choose(4, [A,B,C,D,_,_,_,E], [A,B,C,D,E]).
choose(5, [A,B,C,_,D,E,_,_], [A,B,C,D,E]).
choose(6, [A,B,C,_,D,_,E,_], [A,B,C,D,E]).
choose(7, [A,B,C,_,D,_,_,E], [A,B,C,D,E]).
choose(8, [A,B,C,_,_,D,E,_], [A,B,C,D,E]).
choose(9, [A,B,C,_,_,D,_,E], [A,B,C,D,E]).
choose(10, [A,B,C,_,_,_,D,E], [A,B,C,D,E]).
choose(11, [A,B,_,C,D,E,_,_], [A,B,C,D,E]).
choose(12, [A,B,_,C,D,_,E,_], [A,B,C,D,E]).
choose(13, [A,B,_,C,D,_,_,E], [A,B,C,D,E]).
choose(14, [A,B,_,C,_,D,E,_], [A,B,C,D,E]).
choose(15, [A,B,_,C,_,D,_,E], [A,B,C,D,E]).
choose(16, [A,B,_,C,_,_,D,E], [A,B,C,D,E]).
choose(17, [A,B,_,_,C,D,E,_], [A,B,C,D,E]).
choose(18, [A,B,_,_,C,D,_,E], [A,B,C,D,E]).
choose(19, [A,B,_,_,C,_,D,E], [A,B,C,D,E]).
choose(20, [A,B,_,_,_,C,D,E], [A,B,C,D,E]).
choose(21, [A,_,B,C,D,E,_,_], [A,B,C,D,E]).
choose(22, [A,_,B,C,D,_,E,_], [A,B,C,D,E]).
choose(23, [A,_,B,C,D,_,_,E], [A,B,C,D,E]).
choose(24, [A,_,B,C,_,D,E,_], [A,B,C,D,E]).
choose(25, [A,_,B,C,_,D,_,E], [A,B,C,D,E]).
choose(26, [A,_,B,C,_,_,D,E], [A,B,C,D,E]).
choose(27, [A,_,B,_,C,D,E,_], [A,B,C,D,E]).
choose(28, [A,_,B,_,C,D,_,E], [A,B,C,D,E]).
choose(29, [A,_,B,_,C,_,D,E], [A,B,C,D,E]).
choose(30, [A,_,B,_,_,C,D,E], [A,B,C,D,E]).
choose(31, [A,_,_,B,C,D,E,_], [A,B,C,D,E]).
choose(32, [A,_,_,B,C,D,_,E], [A,B,C,D,E]).
choose(33, [A,_,_,B,C,_,D,E], [A,B,C,D,E]).
choose(34, [A,_,_,B,_,C,D,E], [A,B,C,D,E]).
choose(35, [A,_,_,_,B,C,D,E], [A,B,C,D,E]).
choose(36, [_,A,B,C,D,E,_,_], [A,B,C,D,E]).
choose(37, [_,A,B,C,D,_,E,_], [A,B,C,D,E]).
choose(38, [_,A,B,C,D,_,_,E], [A,B,C,D,E]).
choose(39, [_,A,B,C,_,D,E,_], [A,B,C,D,E]).
choose(40, [_,A,B,C,_,D,_,E], [A,B,C,D,E]).
choose(41, [_,A,B,C,_,_,D,E], [A,B,C,D,E]).
choose(42, [_,A,B,_,C,D,E,_], [A,B,C,D,E]).
choose(43, [_,A,B,_,C,D,_,E], [A,B,C,D,E]).
choose(44, [_,A,B,_,C,_,D,E], [A,B,C,D,E]).
choose(45, [_,A,B,_,_,C,D,E], [A,B,C,D,E]).
choose(46, [_,A,_,B,C,D,E,_], [A,B,C,D,E]).
choose(47, [_,A,_,B,C,D,_,E], [A,B,C,D,E]).
choose(48, [_,A,_,B,C,_,D,E], [A,B,C,D,E]).
choose(49, [_,A,_,B,_,C,D,E], [A,B,C,D,E]).
choose(50, [_,A,_,_,B,C,D,E], [A,B,C,D,E]).
choose(51, [_,_,A,B,C,D,E,_], [A,B,C,D,E]).
choose(52, [_,_,A,B,C,D,_,E], [A,B,C,D,E]).
choose(53, [_,_,A,B,C,_,D,E], [A,B,C,D,E]).
choose(54, [_,_,A,B,_,C,D,E], [A,B,C,D,E]).
choose(55, [_,_,A,_,B,C,D,E], [A,B,C,D,E]).
choose(56, [_,_,_,A,B,C,D,E], [A,B,C,D,E]).

