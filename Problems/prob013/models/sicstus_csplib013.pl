/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Progressive Party Problem
 * Author    : Mats Carlsson
 * 
 * A set of guest boat crews are supposed to visit a set of host boats in
 * six shifts.  The host boats have finite capacity. A guest crew can't
 * visit a host twice.  Two guest crews can't meet twice.
 * 
 * See Smith, B.,Brailsford, S., Hubbard, P., and Williams, H.
 * The progressive party problem: integer linear programming and
 * constraint programming compared.  Constraints 1:119-138, 1996.
 * 
 * Constants:
 * 
 *     spare(I) : the spare capacity of host I
 *     crew(I) : the crew size of guest I
 * 
 * 
 * Variables:
 * 
 *     h(I,T) in 1..13 : the host boat that guest boat I visits at time T
 *     m(I,J,T) #<=> h(I,T) #= h(J,T)
 *     
 * 
 * Problem constraints:
 * 
 *     all_different([h(I,1),...,h(I,6)]) % guest I can't visit a host twice
 *     cumulatives([task(1,1,2,2,h(1,1)), ..., task(6,1,7,2,h(1,6)),
 *                  ...
 *                  task(1,1,2,4,h(13,1)), ..., task(6,1,7,4,h(13,6))],
 *		   [machine(1,10),...,machine(13,4)],
 *		   [bound(upper)])
 *     sum([m(I,J,1)...,m(I,J,6)], #=<, 1) % crews I,J can't meet twice
 * 
 * Redundant constraints (do not seem to help):
 * 
 *     count(H, [h(1,T),...,h(29,T)], #>=, 1)
 * 
 * Asymmetry constraints:
 * 
 *     h(1,1) #< ... #< h(1,6)
 *     I<J & crew(I) #= crew(J) #=> 
 *         (h(I,1) #=< h(J,1) #/\ (h(I,1) #< h(J,1) #\/ h(I,2) #< h(J,2)))
 *
 * | ?- party.
 */
:- module(party, [party/0]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

party :-
	party_variables(6, Vars),
	party_constraints(6, Vars),
	guest_vars(0, 29, Vars, All, []),
	labeling([ff], All),
	format('Guest~t~10|Hosts\n', []),
	pp_party(13, 42, Vars).

guest_vars(G, G, _) --> !.
guest_vars(G0, G, Vars) -->
	{G1 is G0+1},
	{aget(h(G1), Vars, Row)},
	(foreach(X,Row) do [X]),		 
	guest_vars(G1, G, Vars).

pp_party(G, G, _) :- !.
pp_party(I, G, Vars) :-
	J is I+1,
	(guest(J1, _, J) -> true),
	aget(h(J1), Vars, Row),
	(   foreach(H,Row),
	    foreach(H1,Row1)
	do  host(H, _, H1)
	),
	format('~w~t~10|~w\n', [J,Row1]),
	pp_party(J, G, Vars).

party_variables(T, Vars) :-
	Vars = vars(H,M),
	sizes(Hosts, Guests),
	functor(H, h, Guests),
	functor(M, m, Guests),
	h_array(Guests, T, Hosts, H),
	m_array(Guests, T, Vars, M).

party_constraints(Times, Vars) :-
	sizes(Hosts, Guests),
	(   for(G1,1,Guests),
	    param([Vars,domain])
	do  aget(h(G1), Vars, L1),
	    all_distinct(L1)
	),
	host_capacities(Hosts, Guests, Times, Vars),
	(   for(G2,1,Guests),
	    param(Vars)
	do  (   for(H,1,G2-1),
		param([G2,Vars])
	    do  aget(m(G2,H), Vars, L2),
		sum(L2, #=<, 1)
	    )
	),
	first_guest_order(Vars),
	% redundant(Hosts, Guests, Times, Vars), % does not help
	asym_crews(Vars).

% host_capacities(13, 29, 6, Vars)
host_capacities(Hosts, Guests, Times, Vars) :-
	host_cap_tasks(0, Guests, Times, Vars, Tasks, []),
	host_cap_machines(0, Hosts, Mach, []),
	cumulatives(Tasks, Mach, [bound(upper)]).

host_cap_tasks(G, G, _, _) --> !.
host_cap_tasks(G0, G, Times, Vars) -->
	{G1 is G0+1},
	guest_cap_tasks(0, Times, G1, Vars),
	host_cap_tasks(G1, G, Times, Vars).

guest_cap_tasks(T, T, _, _) --> !.
guest_cap_tasks(T0, T, G1, Vars) --> [task(T0,1,T1,CrewSize,Host)],
	{T1 is T0+1},
	{guest(G1, CrewSize, _)},
	{aget(h(G1,T1), Vars, Host)},
	guest_cap_tasks(T1, T, G1, Vars).

host_cap_machines(H, H) --> !.
host_cap_machines(H0, H) --> [machine(H1,Spare)],
	{H1 is H0+1},
	{host(H1, Spare, _)},
	host_cap_machines(H1, H).

first_guest_order(Vars) :-
	aget(h(1), Vars, [H1|Hosts]),
	first_guest_order(Hosts, H1).

first_guest_order([], _).
first_guest_order([H2|Hs], H1) :-
	H1 #< H2,
	first_guest_order(Hs, H2).

% redundant(Hosts, Guests, Times, Vars)
redundant(_, _, 0, _) :- !.
redundant(H, G, T, Vars) :-
	redundant2(G, T, Vars, L),
	redundant(H, L),
	(   for(I,1,H),
	    param(L)
	do  count(I, L, #>=, 1)
	),
	T1 is T-1,
	redundant(H, G, T1, Vars).

redundant2(0, _, _, []) :- !.
redundant2(G, T, Vars, [V|Vs]) :-
	aget(h(G,T), Vars, V),
	G1 is G-1,
	redundant2(G1, T, Vars, Vs).

asym_crews(Vars) :- 
	findall(Crew-Guest, guest(Guest,Crew,_), Pairs),
	keysort(Pairs, Keysorted),
	keyclumped(Keysorted, Keymerged),
	(   foreach(_-Class,Keymerged),
	    param(Vars)
	do  asym_crews1(Class, Vars)
	).

asym_crews1([_], _) :- !.
asym_crews1([G1,G2|Gs], Vars) :-
	aget(h(G1,1), Vars, H11),
	aget(h(G1,2), Vars, H12),
	aget(h(G2,1), Vars, H21),
	aget(h(G2,2), Vars, H22),
	H11 #=< H21,
	H11 #< H21 #\/ H12 #< H22,
	asym_crews1([G2|Gs], Vars).

h_array(0, _, _, _) :- !.
h_array(I, T, Hosts, H) :-
	arg(I, H, Row),
	length(L, T),
	domain(L, 1, Hosts),
	Row =.. [h|L],
	J is I-1,
	h_array(J, T, Hosts, H).

v_array(0, _, _, _, _) :- !.
v_array(I, Hosts, T, Vars, V) :-
	arg(I, V, Row),
	functor(Row, v, Hosts),
	v_array1(I, Hosts, T, Vars, Row),
	I1 is I-1,
	v_array(I1, Hosts, T, Vars, V).

v_array1(_, 0, _, _, _) :- !.
v_array1(I, J, T, Vars, V) :-
	arg(J, V, Row),
	functor(Row, v, T),
	v_array2(I, J, T, Row, Vars),
	J1 is J-1,
	v_array1(I, J1, T, Vars, V).

v_array2(_, _, 0, _, _) :- !.
v_array2(I, J, T, Row, Vars) :-
	arg(T, Row, X),
	aget(h(I,T), Vars, Y),
	Y #= J #<=> X,
	T1 is T-1,
	v_array2(I, J, T1, Row, Vars).


m_array(0, _, _, _) :- !.
m_array(I, T, Vars, M) :-
	I1 is I-1,
	arg(I, M, Row),
	functor(Row, m, I1),
	m_array1(I, I1, T, Vars, Row),
	m_array(I1, T, Vars, M).

m_array1(_, 0, _, _, _) :- !.
m_array1(I, J, T, Vars, M) :-
	arg(J, M, Row),
	functor(Row, m, T),
	m_array2(I, J, T, Row, Vars),
	J1 is J-1,
	m_array1(I, J1, T, Vars, M).

m_array2(_, _, 0, _, _) :- !.
m_array2(I, J, T, Row, Vars) :-
	arg(T, Row, X),
	aget(h(I,T), Vars, Y),
	aget(h(J,T), Vars, Z),
	Y #= Z #<=> X,
	T1 is T-1,
	m_array2(I, J, T1, Row, Vars).

aget(h(I), vars(H,_), L) :-
	arg(I, H, X0),
	X0 =.. [h|L].
aget(h(I,T), vars(H,_), X) :-
	arg(I, H, X0),
	arg(T, X0, X).
aget(m(I,J), vars(_,M), L) :-
	arg(I, M, X0),
	arg(J, X0, X1),
	X1 =.. [m|L].
aget(m(I,J,T), vars(_,M), X) :-
	arg(I, M, X0),
	arg(J, X0, X1),
	arg(T, X1, X).


% First 13 are hosts, remaining 29 are guests.
% boat(BoatNo, Capacity, CrewSize)
boat( 1,  6, 2).
boat( 2,  8, 2).
boat( 3, 12, 2).
boat( 4, 12, 2).
boat( 5, 12, 4).
boat( 6, 12, 4).
boat( 7, 12, 4).
boat( 8, 10, 1).
boat( 9, 10, 2).
boat(10, 10, 2).
boat(11, 10, 2).
boat(12, 10, 3).
boat(13,  8, 4).
boat(14,  8, 2).
boat(15,  8, 3).
boat(16, 12, 6).
boat(17,  8, 2).
boat(18,  8, 2).
boat(19,  8, 4).
boat(20,  8, 2).
boat(21,  8, 4).
boat(22,  8, 5).
boat(23,  7, 4).
boat(24,  7, 4).
boat(25,  7, 2).
boat(26,  7, 2).
boat(27,  7, 4).
boat(28,  7, 5).
boat(29,  6, 2).
boat(30,  6, 4).
boat(31,  6, 2).
boat(32,  6, 2).
boat(33,  6, 2).
boat(34,  6, 2).
boat(35,  6, 2).
boat(36,  6, 2).
boat(37,  6, 4).
boat(38,  6, 5).
boat(39,  9, 7).
boat(40,  0, 2).
boat(41,  0, 3).
boat(42,  0, 4).

% derived facts
% host(Id, SpareCap, BoatNo)
% Total spare cap = 98, guest crews = 94
host( 1, 10, 3).
host( 2, 10, 4).
host( 3,  9, 8).
host( 4,  8, 5).
host( 5,  8, 6).
host( 6,  8, 7).
host( 7,  8, 9).
host( 8,  8, 10).
host( 9,  8, 11).
host(10,  7, 12).
host(11,  6, 2).
host(12,  4, 1).
host(13,  4, 13).

% guest(Id, CrewSize, BoatNo)
guest( 1, 7, 39).
guest( 2, 6, 16).
guest( 3, 5, 22).
guest( 4, 5, 28).
guest( 5, 5, 38).
guest( 6, 4, 19).
guest( 7, 4, 21).
guest( 8, 4, 23).
guest( 9, 4, 24).
guest(10, 4, 27).
guest(11, 4, 30).
guest(12, 4, 37).
guest(13, 4, 42).
guest(14, 3, 15).
guest(15, 3, 41).
guest(16, 2, 14).
guest(17, 2, 17).
guest(18, 2, 18).
guest(19, 2, 20).
guest(20, 2, 25).
guest(21, 2, 26).
guest(22, 2, 29).
guest(23, 2, 31).
guest(24, 2, 32).
guest(25, 2, 33).
guest(26, 2, 34).
guest(27, 2, 35).
guest(28, 2, 36).
guest(29, 2, 40).

% sizes(Hosts, Guests)
sizes(13, 29).
