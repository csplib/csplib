/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : The Rehearsal Problem
 * Author    : Mats Carlsson
 *
 * | ?- rehearsal(concert).
 * | ?- rehearsal(film1).
 * | ?- rehearsal(film2).
 */

:- module(rehearsal, [rehearsal/1]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

rehearsal(ID) :-
	system(ID, Cost, Dur, TaskOrder, Pieces, Players),
	search(Cost, Dur, TaskOrder),
	print_solution(Pieces, Players).

print_solution(Pieces, Players) :-
	piece_events(Pieces, 0, L1, L2),
	player_events(Players, 0, L2, []),
	sort(L1, L3),		% for end < leave < arrive < start
	(   foreach(Key-Val,L3)
	do  print_event(Val, Key)
	),
	print_players(Players, 0, 0).

piece_events([], _) --> [].
piece_events([task(S,_,E,_,_)|L], I) --> [S-start(J),E-end(J)],
	{J is I+1},
	piece_events(L, J).

player_events([], _) --> [].
player_events([player(S,_,E,_,_)|L], I) --> [S-rive(J),E-leave(J)],
	{J is I+1},
	player_events(L, J).

print_event(start(P), At) :-
	format('~d: piece ~d starts.\n', [At,P]).
print_event(end(P), At) :-
	format('~d: piece ~d ends.\n', [At,P]).
print_event(rive(P), At) :-
	format('~d: player ~d arrives.\n', [At,P]).
print_event(leave(P), At) :-
	format('~d: player ~d leaves.\n', [At,P]).

print_players([], _, Cost) :-
	format('Total cost ~d.\n', [Cost]).
print_players([player(_,D,_,Duty,Sal)|L], I, Cost1) :-
	J is I+1,
	Idle is (D-Duty),
	Inc is Idle*Sal,
	format('Player ~d playing ~d, idle ~d, cost ~d.\n',
	       [J,Duty,Idle,Inc]),
	Cost2 is Cost1+Inc,
	print_players(L, J, Cost2).
	

search(Cost, Dur, TaskOrder) :-
	min_max(1000000, Cost, [], TaskOrder, Dur, []).

min_max(Cost0, Cost, _, TaskOrder, Dur, Ora1) :-
	Cost #< Cost0,
	findall(f(TaskOrder,Cost,Ora2), (left(TaskOrder,0,Dur,Ora1,Ora2)->true), [f(TO1,C1,O2)]), !,
	min_max(C1, Cost, TO1, TaskOrder, Dur, O2).
min_max(Cost, Cost, TaskOrder, TaskOrder, _, _).
	
left([_], _, _, [], []) :- !.
left(L1, Est, Lct, Ora1, [N|Ns]) :-
	ora_head_tail(Ora1, M, Ms),
	Item = task(Est,_,Est1,_,_),
	select_nth(Item, L1, L2, 0, M, N),
	(M=:=N -> Ora2=Ms ; Ora2=[]),
	right(L2, Est1, Lct, Ora2, Ns).

ora_head_tail([], 0, []).
ora_head_tail([M|T], M, T).

right([_], _, _, [], []) :- !.
right(L1, Est, Lct, Ora1, [N|Ns]) :-
	ora_head_tail(Ora1, M, Ms),
	Item = task(Lct1,_,Lct,_,_),
	select_nth(Item, L1, L2, 0, M, N),
	(M=:=N -> Ora2=Ms ; Ora2=[]),
	left(L2, Est, Lct1, Ora2, Ns).

system(ID, Cost, Dur, TaskOrder, Items, Players) :-
	durations(ID, Ds),
	sumlist(Ds, Dur),
	(   foreach(D,Ds),
	    foreach(S,Ss),
	    foreach(E,Es),
	    foreach(task(S,D,E,1,0),Items)
	do  true
	),
	domain(Ss, 0, Dur),
	domain(Es, 0, Dur),
	findall(L, plays_in_pieces(ID,L,_), PieceMat),
	findall(C, plays_in_pieces(ID,_,C), Salaries),
	(   foreach(PieceRow,PieceMat),
	    foreach(Sal,Salaries),
	    foreach(player(Min,_,Max,Duty1,Sal),Players),
	    param([Ss,Es,Ds,Dur])
	do  active(PieceRow, Ss, Es, Ds, Ss1, Es1, Ds1),
	    minimum(Min, Ss1),
	    maximum(Max, Es1),
	    sumlist(Ds1, Duty1)
	),
	costs(Players, Salaries, 0, Duty, CVs, []),
	(   foreach(K-V1,CVs),
	    foreach(K,Coeffs),
	    foreach(V1,Vars)
	do  true
	),
	scalar_product([-1|Coeffs], [Duty|Vars], #=, Cost),
	% non-overlapping pieces
	cumulative(Items, [/*global(true) does not pay off*/]),
	% heuristic piece order
	transpose(PieceMat, PieceMatT),
	tag_by_occ(PieceMatT, Items, L1, []),
	keysort(L1, L2),
	(   foreach(_-V2,L2),
	    foreach(V2,TaskOrder)
	do  true
	),
	% symmetries
	TaskOrder = [task(S1,_,_,_,_),task(S2,_,_,_,_)|_],
	S1 #< S2,
	% redundant optimality constraints
	findall(R, redundancy(PieceMat,PieceMatT,R), Red),
	redundant(Red, Ss, Es, Players).

/*
Redundancy idea 1: implied constraints.
If player P1 plays a subset of the pieces played by player P2,
then [min(P1),max(P1)] must be contained in [min(P2),max(P2)].

Redundancy idea 2: optimality constraints.
Find Piece1 and Piece2 have identical columns except for some players Ps,
who all play Piece2 but not Piece1. Then for any solution where:

[1]  all P in Ps : min(P) < Piece1 < Piece2

there is a solution which is no worse where:

[2]  all P in Ps : min(P) < Piece2 < Piece1

Similarly, for any solution where:

[3]  all P in Ps : Piece2 < Piece1 < max(P)

there is a solution which is no worse where:

[4]  all P in Ps : Piece1 < Piece2 < max(P)

So exclude [1] and [3], taking care to not wipe out
a solution that matches both [1] and [3].
*/
redundancy(_, MatT, swap(Players,Piece1,Piece2)) :-
	select_nth(Col1, MatT, _, 1, 1, Piece1),
	select_nth(Col2, MatT, _, 1, 1, Piece2),
	Piece1=\=Piece2,
	Col1\==Col2,
	subset_01(Col1, Col2, 0, Players).
redundancy(Mat, _, subset(Player1,Player2,D)) :-
	select_nth(Row1, Mat, _, 1, 1, Player1),
	select_nth(Row2, Mat, _, 1, 1, Player2),
	Player1=\=Player2,
	subset_01_diff(Row1, Row2, 0, D).
% % didn't help
% redundancy(Mat, _, disjoint(Player1,Player2)) :-
% 	select_nth(Row1, Mat, _, 1, 1, Player1),
% 	select_nth(Row2, Mat, _, 1, 1, Player2),
% 	Player1 < Player2,
% 	disjoint_01(Row1, Row2).

subset_01_diff([], [], D, D).
subset_01_diff([X|L1], [Y|L2], D1, D3) :-
	X =< Y,
	D2 is D1+(Y-X),
	subset_01_diff(L1, L2, D2, D3).

subset_01([], [], _, []).
subset_01([X|L1], [X|L2], I, L3) :- !,
	J is I+1,
	subset_01(L1, L2, J, L3).
subset_01([0|L1], [1|L2], I, [J|L3]) :-
	J is I+1,
	subset_01(L1, L2, J, L3).

% disjoint_01([], []).
% disjoint_01([X|L1], [Y|L2]) :-
% 	X+Y < 2,
% 	disjoint_01(L1, L2).

redundant([], _, _, _).
redundant([subset(Player1,Player2,_)|Red], Ss, Es, MinMax) :- !,
	nth1(Player1, MinMax, player(Min1,_D1,Max1,_,_)),
	nth1(Player2, MinMax, player(Min2,_D2,Max2,_,_)),
	Min1 #>= Min2, % helps
	Max1 #=< Max2, % helps
	% _D1 #=< _D2, % does not help
	redundant(Red, Ss, Es, MinMax).
% redundant([disjoint(Player1,Player2)|Red], Ss, Es, MinMax) :- !,
% 	nth1(Player1, MinMax, player(Min1,_,Max1,_,_)),
% 	nth1(Player2, MinMax, player(Min2,_,Max2,_,_)),
% 	Min1 #\= Min2,
% 	Max1 #\= Max2,
% 	redundant(Red, Ss, Es, MinMax).
redundant([swap(Players,Piece1,Piece2)|Red], Ss, Es, MinMax) :- !,
	mins_maxs(Players, MinMax, Mins, Maxs),
	maximum(Min, Mins),
	minimum(Max, Maxs),
	nth1(Piece1, Ss, S1),
	nth1(Piece2, Ss, S2),
	nth1(Piece1, Es, E1),
	S1 #=< S2 #=> E1 #=< Min,
	Min #= S2 #/\ S2 #=< S1 #=> S1 #>= Max, % not in the paper
	redundant(Red, Ss, Es, MinMax).

mins_maxs([], _, [], []).
mins_maxs([P|Ps], MinMax, [Min|Mins], [Max|Maxs]) :-
	nth1(P, MinMax, player(Min,_,Max,_,_)),
	mins_maxs(Ps, MinMax, Mins, Maxs).	

% order: (i) min #occs, (ii) max dur
tag_by_occ([], []) --> [].
tag_by_occ([Col|Mat], [Item|Items]) --> [key(Occ,NDur)-Item],
	{Item = task(_,D,_,_,_)},
	{NDur is -D},
	{sumlist(Col, Occ)},
	tag_by_occ(Mat, Items).

active([], [], [], [], [], [], []).
active([1|Row], [S|Ss], [E|Es], [D|Ds], [S|Ss1], [E|Es1], [D|Ds1]) :- !,
	active(Row, Ss, Es, Ds, Ss1, Es1, Ds1).
active([0|Row], [_|Ss], [_|Es], [_|Ds], Ss1, Es1, Ds1) :-
	active(Row, Ss, Es, Ds, Ss1, Es1, Ds1).

costs([], [], Duty, Duty) --> [].
costs([player(Min,Dur,Max,Duty,_)|MM], [Sal|Sals], Duty1, Duty3) --> [Sal-Dur],
	{Dur in Duty..sup},
	{Dur #= Max-Min},
	{Duty2 is Duty1+Sal*Duty},
	costs(MM, Sals, Duty2, Duty3).

select_nth(X, [Y|R],     R, K, Min, K) :-
	K >= Min,
	X = Y.
select_nth(X, [A|L], [A|R], I, Min, K) :-
	J is I+1,
	select_nth(X, L, R, J, Min, K).

:- discontiguous
	plays_in_pieces/3,
	durations/2.

plays_in_pieces(concert, [1,1,0,1,0,1,1,0,1], 1).
plays_in_pieces(concert, [1,1,0,1,1,1,0,1,0], 1).
plays_in_pieces(concert, [1,1,0,0,0,0,1,1,0], 1).
plays_in_pieces(concert, [1,0,0,0,1,1,0,0,1], 1).
plays_in_pieces(concert, [0,0,1,0,1,1,1,1,0], 1).

durations(concert,       [2,4,1,3,3,2,5,7,6]).

plays_in_pieces(film1, [1,1,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0], 10).
plays_in_pieces(film1, [1,1,1,0,0,0,1,1,0,1,0,0,1,1,1,0,1,0,0,1],  4).
plays_in_pieces(film1, [0,1,1,0,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0],  5).
plays_in_pieces(film1, [0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],  5).
plays_in_pieces(film1, [0,1,0,0,0,0,1,1,0,0,0,1,0,1,0,0,0,1,1,1],  5).
plays_in_pieces(film1, [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0], 40).
plays_in_pieces(film1, [0,0,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0],  4).
plays_in_pieces(film1, [0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0], 20).

durations(film1,       [2,1,1,1,1,3,1,1,1,2,1,1,2,1,2,1,1,2,1,1]).

plays_in_pieces(film2, [0,0,1,0,0,0,0,0,1,1,1,1,0], 40).
plays_in_pieces(film2, [1,1,0,0,1,1,1,1,1,1,1,0,1], 20).
plays_in_pieces(film2, [0,1,0,0,0,0,0,1,0,0,0,0,0], 20).
plays_in_pieces(film2, [1,0,0,1,1,1,1,1,1,1,0,0,1], 10).
plays_in_pieces(film2, [0,0,0,1,0,0,0,0,0,1,0,0,0],  5).
plays_in_pieces(film2, [1,0,0,0,0,1,1,0,1,1,1,1,0], 10).
plays_in_pieces(film2, [0,1,0,0,1,0,0,0,1,1,1,0,0],  5).
plays_in_pieces(film2, [0,0,0,0,0,1,0,0,0,1,0,0,0],  4).
plays_in_pieces(film2, [0,0,0,0,0,0,0,0,0,0,1,0,1],  5).
plays_in_pieces(film2, [0,0,0,0,0,0,0,0,1,1,0,0,0],  4).

durations(film2,       [1,1,1,1,3,1,1,1,1,1,1,1,1]).
