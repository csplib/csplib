/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Social Golfer Problem
 * Author    : Mats Carlsson
 *
 * We have 32 golfers, individual play.
 * We will golf for W weeks.
 * Set up the foursomes so that each person only golfs with the same
 * person once.
 *
 * | ?- golf(8,4,9,[min],bycolall,bounds).
 */

:- module(golf, [golf/6]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

golf(G, S, W, LabelOpt, VarOrder, Consistency) :-
	Opt = [consistency(Consistency)],
	golfer(G, S, W, Schedule, Byrow, Bycol, Opt),
	var_order(VarOrder, Byrow, Bycol, All),
	(   foreach(Set,All),
	    param(LabelOpt)
	do  labeling(LabelOpt, Set)
	),
	(   foreach(Round,Schedule),
	    count(Wk,1,_)
	do  format('Week ~d:\n', [Wk]),
	    (   foreach(Four,Round)
	    do  format('                    ~d ~d ~d ~d\n', Four)
	    )
	).

var_order(bycol, _, All, All).
var_order(byrow, All, _, All).
var_order(bycolall, _, Cols, [All]) :-
	append(Cols, All).
var_order(byrowall, Rows, _, [All]) :-
	append(Rows, All).

golfer(G, S, W, Schedule, PlayersByRow, PlayersByCol, Opt) :-
	schedule(0, G, S, W, Schedule, PlayersByRow, PlayersByCol, Opt),
	Schedule = [FirstS|RestS],
	append(FirstS, Players),
	labeling([enum], Players), !,
	(   foreach(Week,RestS),
	    param(S)
	do  (   foreach([P|Ps],Week),
		param(S)
	    do  P/S #= Q0,
		(   foreach(P1,Ps),
		    fromto(Q0,Q1,Q2,_),
		    param(S)
		do  P1/S #= Q2,
		    Q1 #< Q2
		)
	    ),
	    seed_week(0, S, Week)
	),
	ordered_players_by_week(PlayersByRow),
	players_meet_disjoint(Schedule, G, S, Opt),
	first_s_alldiff(0, S, RestS, Opt).


schedule(W, _, _, W, [], [], [], _) :- !.
schedule(I, G, S, W, [Week|Schedule], [ByRow|ByRows], [ByCol|ByCols], Opt) :-
	(   for(_,1,G),
	    foreach(Group,Week),
	    param([G,S])
	do  length(Group, S),
	    GS is G*S-1,
	    domain(Group, 0, GS)
	),
	append(Week, ByRow),
	all_different(ByRow, Opt),
	transpose(Week, WeekT),
	append(WeekT, ByCol),
	J is I+1,
	schedule(J, G, S, W, Schedule, ByRows, ByCols, Opt).

players_meet_disjoint(Schedule, G, S, Opt) :-
	append(Schedule, Groups),
	groups_meets(Groups, Tuples, [], MeetVars, []),
	GS is G*S,
	(   foreach([A,B,C],Tuples),
	    param([GS,Opt])
	do  scalar_product([GS,1], [A,B], #=, C, Opt)
	),
	all_distinct(MeetVars, Opt).

groups_meets([], Tuples, Tuples) --> [].
groups_meets([Group|Groups], Tuples1, Tuples3) -->
	group_meets(Group, Tuples1, Tuples2),
	groups_meets(Groups, Tuples2, Tuples3).

group_meets([], Tuples, Tuples) --> [].
group_meets([P|Ps], Tuples1, Tuples3) -->
	group_meets(Ps, P, Tuples1, Tuples2),
	group_meets(Ps, Tuples2, Tuples3).

group_meets([], _, Tuples, Tuples) --> [].
group_meets([Q|Qs], P, [[P,Q,PQ]|Tuples1], Tuples2) --> [PQ],
	group_meets(Qs, P, Tuples1, Tuples2).

seed_week(S, S, Week) :- !,
	S1 is S-1,
	seed_week(Week, S1).
seed_week(I, S, [[I|_]|Week]) :-
	J is I+1,
	seed_week(J, S, Week).

seed_week([], _).
seed_week([[J|_]|Week], I) :-
	I #< J,
	seed_week(Week, J).

ordered_players_by_week([W|Ws]) :-
	ordered_players_by_week(Ws, W).

ordered_players_by_week([], _).
ordered_players_by_week([W|Ws], V) :-
	W = [_,Y|_],
	V = [_,X|_],
	X #< Y,
	ordered_players_by_week(Ws, W).

first_s_alldiff(S, S, _Schedule, _) :- !.
first_s_alldiff(I, S, Schedule, Opt) :-
	(   foreach(Week,Schedule),
	    foreach(Ith,Part),
	    param(I)
	do  nth0(I, Week, [_|Ith])
	),
	append(Part, Conc),
	all_different(Conc, Opt),
	J is I+1,
	first_s_alldiff(J, S, Schedule, Opt).

