%
% Solitaire Battleships
% Sample ECLiPSe solution by Joachim Schimpf, 2016
% under Creative Commons Attribution 4.0 International License.
%
% This is problem 114 in CSPLIB (http://csplib.org/Problems/prob014)
% see also http://en.wikipedia.org/wiki/Battleship_(puzzle)
%
% Place a given number of ships of different sizes on the grid.
% A ship is a sequence of one (a submarine) to four (a battleship)
% consecutive 1s in the grid, arranged either horizontally or
% vertically.  Ships do not touch each other, not even diagonally.
% For each row and each column, the number of 1s in that row/column
% is given.  For some coordinates, a "hint" is given, which can be either:
%	w(ater)		field is 0
%	c(ircle)	field contains a submarine
%	l(eft)		field is the left end of a longer ship
%	r(ight)		field is the right end of a longer ship
%	t(op)		field is the top end of a longer ship
%	b(ottom)	field is the bottom end of a longer ship
%	m(iddle)	field is an inner part (not the end) of a ship
% Sample run:
% 
% $ eclipse -f battleships.ecl -e 'battleships(example,_,_)'
% 
%  . . . . . . x . . .
%  . . . x . . x . x .
%  . . . . . . x . . .
%  . x . . . . . . . .
%  . x . x x x x . x .
%  . . . . . . . . . .
%  . . . . . . x x . .
%  . x x x . . . . . .
%  . . . . . x . . . .
%  . . . . . . . . x x
% 

:- lib(ic).
:- lib(ic_global).

% Include a file with problem instances:
%:- include(battleship_instances).
% Or specify your own problem instance here:
problem(example,
	[4:1,3:2,2:3,1:4],				% Fleet [ShipSize:Amount]
	[1,3,1,1,6,0,2,3,1,2],				% Row sums
	[0,3,1,3,1,2,5,1,3,1],				% Column sums
	[c@[2,9],t@[4,2],m@[5,6],l@[8,2],r@[10,10]]	% Hints
    ).


battleships(Name, Ships, Grid) :-
	problem(Name, Fleet, RowSums, ColSums, Hints),
	grid_constraints(RowSums, ColSums, Hints, Grid),
	place_ships(Fleet, Grid, Ships),
	print_grid(Grid).


% Deterministically set up grid constraints
grid_constraints(RowSums, ColSums, Hints, Grid) :-
	length(RowSums, M),
	length(ColSums, N),
	dim(Grid, [M,N]),
	Grid #:: 0..1,
	( foreach(RowSum,RowSums), for(I,1,M), param(Grid,N) do
	    sum(Grid[I,1..N]) #= RowSum
	),
	( foreach(ColSum,ColSums), for(J,1,N), param(Grid,M) do
	    sum(Grid[1..M,J]) #= ColSum
	),
	( foreach(What@[I,J],Hints), param(Grid) do
	    hint(What, Pattern),
	    place_pattern(Pattern, I, J, Grid)
	).


% Nondeterministically place the fleet
% To remove symmetry, we impose lex order on coordinates of same size ships
place_ships(Fleet, Grid, Ships) :-
	sort(1, >=, Fleet, OrderedFleet),
	(
	    foreach(Size:Num,OrderedFleet),
	    fromto(Ships,Ps1,Ps3,[]),
	    param(Grid)
	do
	    (
		for(_,1,Num),			% place Num ships of Size
		fromto(Ps1,[Ship|Ps2],Ps2,Ps3),
		fromto([0,0],[I0,J0],[I,J],_),	% ordered coordinates
		param(Size,Grid)
	    do
		Ship = ship(_,[I,J],_),
		lex_lt([I0,J0], [I,J]),
		place_ship_nondet(Size, Ship, Grid)
	    )
	).


% Nondeterministically place one ship of Size
place_ship_nondet(Size, ship(Size,[I,J],Vertical), Grid) :-
	dim(Grid, [M,N]),
	between(1, M, 1, I),
	between(1, N, 1, J),
	ship(Size, HorizontalPattern),
	( Vertical=0, HorizontalPattern = Pattern
	; Vertical=1, Size>1, transpose(HorizontalPattern, Pattern)
	),
	place_pattern(Pattern, I, J, Grid).


% Place a ship or hint pattern on the grid at [I,J]
place_pattern(Pattern, I, J, Grid) :-
	dim(Grid, [M,N]),
	( foreachelem(Given,Pattern,[K,L]), param(Grid,I,J,M,N) do
	    X is I+K-2, Y is J+L-2,
	    ( 1=<X,X=<M, 1=<Y,Y=<N ->
		arg([X,Y], Grid, Given)
	    ;
		Given = 0	% field outside grid, can't be 1
	    )
	).


transpose(P, T) :-
	dim(P, [M,N]),
	dim(T, [N,M]),
	( foreachelem(X,P,[I,J]), param(T) do
	    arg([J,I], T, X)
	).

% Shapes of the different ships (the top left 1 is the reference coordinate)
ship(1, [](
	[](0,0,0),
	[](0,1,0),
	[](0,0,0))).
ship(2, [](
	[](0,0,0,0),
	[](0,1,1,0),
	[](0,0,0,0))).
ship(3, [](
	[](0,0,0,0,0),
	[](0,1,1,1,0),
	[](0,0,0,0,0))).
ship(4, [](
	[](0,0,0,0,0,0),
	[](0,1,1,1,1,0),
	[](0,0,0,0,0,0))).
ship(5, [](
	[](0,0,0,0,0,0,0),
	[](0,1,1,1,1,1,0),
	[](0,0,0,0,0,0,0))).

% What the hints mean
hint(w, [](
	[](_,_,_),
	[](_,0,_),
	[](_,_,_))).
hint(c, [](
	[](0,0,0),
	[](0,1,0),
	[](0,0,0))).
hint(m, [](
	[](0,V,0),
	[](H,1,H),
	[](0,V,0))) :- V #\= H.
hint(t, [](
	[](0,0,0),
	[](0,1,0),
	[](0,1,0))).
hint(b, [](
	[](0,1,0),
	[](0,1,0),
	[](0,0,0))).
hint(l, [](
	[](0,0,0),
	[](0,1,1),
	[](0,0,0))).
hint(r, [](
	[](0,0,0),
	[](1,1,0),
	[](0,0,0))).


print_grid(Grid) :-
	( foreachelem(X,Grid,[_,J]) do
	    ( J==1 -> nl ; true ),
	    ( var(X) -> write(' ?') ; X==1 -> write(' x') ; write(' .') )
	), nl.

