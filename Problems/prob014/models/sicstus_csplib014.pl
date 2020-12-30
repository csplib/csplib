/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Solitaire Battleships
 * Author    : Mats Carlsson
 *
 * | ?- torpedo(id2794).
 */

:- module(torpedo, [torpedo/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

torpedo(ID) :-
	data(ID, PerRowData, PerColData, ShipData),
	length(PerRowData, NRows),
	length(PerColData, NCols),
	% set up domains for the battleship variables
	% a zero tally => that row or column must be empty
	(   foreach(X1,PerColData),
	    count(J1,1,_),
	    fromto(NoCols1,S0a,Sa,[])
	do  (X1=:=0 -> S0a = [J1|Sa] ; S0a = Sa)
	),
	list_to_fdset(NoCols1, NoCols2),
	fdset_parts(XDom1, 1, NCols, []),
	fdset_subtract(XDom1, NoCols2, XDom2),
	(   foreach(X2,PerRowData),
	    count(J2,1,_),
	    fromto(NoRows1,S0b,Sb,[])
	do  (X2=:=0 -> S0b = [J2|Sb] ; S0b = Sb)
	),
	list_to_fdset(NoRows1, NoRows2),
	fdset_parts(YDom1, 1, NRows, []),
	fdset_subtract(YDom1, NoRows2, YDom2),
	(   foreach(X3,ShipData),
	    fromto(TShips1,S0c,S1c,[]),
	    fromto(Coords,C0,C1,[]),
	    param([NCols,NRows,XDom2,YDom2])
	do  ship(X3, NCols, NRows, XDom2, YDom2, S0c, S1c, C0, C1)
	),
	% constrain wrt. the row and column tallies
	(   foreach(K-V,Coords),
	    foreach(K,Xs),
	    foreach(V,Ys)
	do  true
	),
	(   foreach(X4,PerColData),
	    foreach(J3-X4,XC),
	    count(J3,1,_)
	do  true
	),
	(   foreach(X5,PerRowData),
	    foreach(J4-X5,YC),
	    count(J4,1,_)
	do  true
	),
	global_cardinality(Xs, XC),
	global_cardinality(Ys, YC),
	% constrain ships to be non-adjacent
	(   foreach(_-Ship,TShips1),
	    foreach(Ship,Ships),
	    fromto(Vars,[W,H,X6,Y6|S],S,[])
	do  Ship = ship(X6,W,Y6,H)
	),
	disjoint2(Ships),
	% break symmetries: lex order ships of the same class
	keysort(TShips1, TShips2),
	keyclumped(TShips2, Groups),
	(   foreach(_-Group,Groups)
	do  (   foreach(ship(X7,_,Y7,_),Group),
		foreach([X7,Y7],Origs)
	    do  true
	    ),
	    lex_chain(Origs, [op(#<),global(true)])
	),
	% search: largest ship first
	labeling([bisect], Vars),
	% display solution
	draw(Ships, NRows, NCols).

ship(1, _NCols, _NRows, XDom2, YDom2, [1-ship(X,2,Y,2)|Ships], Ships, [X-Y|Coords], Coords) :-
	X in_set XDom2,
	Y in_set YDom2.
ship(2, NCols, NRows, XDom2, YDom2, [2-ship(X,W,Y,H)|Ships], Ships, [X-Y,X1-Y1|Coords], Coords) :-
	X in_set XDom2,
	Y in_set YDom2,
	W in {2,3},
	H in {2,3},
	X+W #=< NCols+2,
	Y+H #=< NRows+2,
	H #= 2 #<=> Horiz,
	W #= 2 #<=> Vert,
	Horiz #\ Vert,
	X1 #= X+Horiz,
	Y1 #= Y+Vert.
ship(3, NCols, NRows, XDom2, YDom2, [3-ship(X,W,Y,H)|Ships], Ships, [X-Y,X1-Y1,X2-Y2|Coords], Coords) :-
	X in_set XDom2,
	Y in_set YDom2,
	W in {2,4},
	H in {2,4},
	X+W #=< NCols+2,
	Y+H #=< NRows+2,
	H #= 2 #<=> Horiz,
	W #= 2 #<=> Vert,
	Horiz #\ Vert,
	X1 #= X+Horiz,
	Y1 #= Y+Vert,
	X2 #= X1+Horiz,
	Y2 #= Y1+Vert.
ship(4, NCols, NRows, XDom2, YDom2, [4-ship(X,W,Y,H)|Ships], Ships, [X-Y,X1-Y1,X2-Y2,X3-Y3|Coords], Coords) :-
	X in_set XDom2,
	Y in_set YDom2,
	W in {2,5},
	H in {2,5},
	X+W #=< NCols+2,
	Y+H #=< NRows+2,
	H #= 2 #<=> Horiz,
	W #= 2 #<=> Vert,
	Horiz #\ Vert,
	X1 #= X+Horiz,
	Y1 #= Y+Vert,
	X2 #= X1+Horiz,
	Y2 #= Y1+Vert,
	X3 #= X2+Horiz,
	Y3 #= Y2+Vert.

draw(Ships, NRows, NCols) :-
	format('+~*c+\n', [NCols,0'-]),
	draw_lines(0, NRows, NCols, Ships),
	format('+~*c+\n', [NCols,0'-]).

draw_lines(NR, NR, _, _) :- !.
draw_lines(I,  NR, NC, Ships) :-
	R is I+1,
	ascii_line(0, NC, R, Ships, String, "|\n"),
	format([0'||String], []),
	draw_lines(R, NR, NC, Ships).

ascii_line(NC, NC, _, _) --> !.
ascii_line(I, NC, R, Ships) -->
	{J is I+1},
	ascii_cell(R, J, Ships),
	ascii_line(J, NC, R, Ships).

ascii_cell(R, J, Ships) -->
	{Template = ship(X,W,Y,H),
	 member(Template, Ships),
	 X=<J, J<X+W-1,
	 Y=<R, R<Y+H-1}, !,
	"#".
ascii_cell(_, _, _) --> " ".

% A couple of items from Moshe Rubin's
% "List of unsolvable Solitaire Battleship boards"

:- dynamic
	data/3.
data(id113, % said to have 70 solutions
     [2,4,3,3,2,4,1,1,0,0],
     [0,5,0,2,2,3,1,3,2,2],
     [4,3,3,2,2,2,1,1,1,1]).
data(id1337, % said to have 49874 solutions
     [1,3,2,2,2,2,2,3,1,2],
     [3,0,4,0,3,0,3,1,2,4],
     [4,3,3,2,2,2,1,1,1,1]).
data(id2794, % said to have 1 solution
     [0,0,1,4,1,5,2,1,6,0],
     [1,3,3,1,1,4,0,1,1,5],
     [4,3,3,2,2,2,1,1,1,1]).
