/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Warehouse Location Problem
 * Author    : Mats Carlsson
 *
 * | ?- warehouse(heur,p1).
 */

:- module(warehouse, [warehouse/2]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

warehouse(cost, Key) :-
	warehouse(Key, Warehouses, Cost, _),
	varorder(posofmin, Key, Warehouses, Vars),
	cost_labeling(Vars, Cost),
	format('warehouses=~w, cost=~w\n', [Warehouses,Cost]).
warehouse(heur, Key) :-
	warehouse(Key, Warehouses, Cost, LBW),
	varorder(maxregret, Key, Warehouses, VarRows),
	minimize(heur_labeling(VarRows, [], 0, LBW), Cost),
	format('warehouses=~w, cost=~w\n', [Warehouses,Cost]).

cost_labeling(Vars, Cost) :-
	labeling([bisect], [Cost]),
	labeling([], Vars).

heur_labeling([], _, _, _).
heur_labeling([Var-Row|Rest], Set, Size, LBW) :-
	Size < LBW, !,
	member(_-Var, Row),
	(   fdset_member(Var, Set) ->
	    Set2 = Set,
	    Size2 = Size
	;   fdset_add_element(Set, Var, Set2),
	    Size2 is Size+1
	),
	heur_labeling(Rest, Set2, Size2, LBW).
heur_labeling([Var-Row|Rest], Set, Size, LBW) :-
	member(_-Var, Row),
	fdset_member(Var, Set),
	heur_labeling(Rest, Set, Size, LBW).
heur_labeling([Var-Row|Rest], Set, Size, LBW) :-
	member(_-Var, Row),
	\+fdset_member(Var, Set),
	fdset_add_element(Set, Var, Set2),
	Size2 is Size+1,
	heur_labeling(Rest, Set2, Size2, LBW).

warehouse(Key, Warehouses, Cost, LBW) :-
	problem(Key, Capacities, BuildCost, Matrix),
	length(Matrix, NStores),
	length(Warehouses, NStores),
	(   foreach(Cap,Capacities),
	    foreach(J-N,Keylist),
	    foreach(B,Binaries),
	    count(J,1,_)
	do  N in 0..Cap,
	    B #= min(N,1)
	),
	build_cost_lb(Capacities, NStores, BuildCost, LBW, LBC),
	BCost #>= LBC,
	BCost + GCCOST #= Cost,
	(   foreach(_,Binaries),
	    foreach(BuildCost,BuildCosts),
	    param(BuildCost)
	do  true
	),
	scalar_product(BuildCosts, Binaries, #=, BCost),
	global_cardinality(Warehouses, Keylist, [cost(GCCOST,Matrix)]).

build_cost_lb(Cap1, Demand, BC, LBW, LBC) :-
	(   foreach(X,Cap1),
	    foreach(Y-0,Cap2)
	do  Y is -X
	),
	keysort(Cap2, Cap3),
	build_cost_lb(Demand, Cap3, Cap4),
	length(Cap3, Len3),
	length(Cap4, Len4),
	LBW is Len3-Len4,
	LBC is BC*LBW.

build_cost_lb(Demand) --> {Demand=<0}, !.
build_cost_lb(Demand1) --> [C-_],
	{Demand2 is Demand1+C},
	build_cost_lb(Demand2).

varorder(posofmin, Key, Warehouses, Vars) :-
	problem(Key, _, _, Matrix),
	(   foreach(Row,Matrix),
	    foreach((Ix,NegRegret),Keys)
	do  min_member(Min, Row),
	    nth1(Ix, Row, Min),
	    select(Min, Row, Rest),
	    min_member(Min2, Rest),
	    NegRegret is Min-Min2
	), !,
	(   foreach(K-V1,KL1),
	    foreach(K,Keys),
	    foreach(V1,Warehouses)
	do  true
	),
	keysort(KL1, KL2),
	(   foreach(_-V2,KL2),
	    foreach(V2,Vars)
	do  true
	).
varorder(maxregret, Key, Warehouses, VarRows2) :-
	problem(Key, _, _, Matrix1),
	(   foreach(Row1,Matrix1),
	    foreach(Row3,Matrix2)
	do  (   foreach(Va,Row1),
		foreach(Va-J,Row2),
		count(J,1,_)
	    do  true
	    ),
	    keysort(Row2, Row3)
	),
	(   foreach(K,Warehouses),
	    foreach(Vb,Matrix2),
	    foreach(NegRegret-(K-Vb),KL1)
	do  Vb = [V1-_,V2-_|_],
	    NegRegret is V1-V2
	),
	keysort(KL1, KL2),
	(   foreach(_-Vc,KL2),
	    foreach(Vc,VarRows2)
	do  true
	).

% problem(ID, Capacities, BuildCosts, CostMatrix).
:- dynamic problem/4.
problem(p1,
	[1,4,2,1,3],
	30,
	[[20,24,11,25,30],
	 [28,27,82,83,74],
	 [74,97,71,96,70],
	 [ 2,55,73,69,61],
	 [46,96,59,83, 4],
	 [42,22,29,67,59],
	 [ 1, 5,73,59,56],
	 [10,73,13,43,96],
	 [93,35,63,85,46],
	 [47,65,55,71,95]]).

