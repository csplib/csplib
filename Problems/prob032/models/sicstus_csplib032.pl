/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Conway's game of life
 *             The goal is to find a 12x12 still-life pattern with 76 
 *             filled squares.
 * Author    : Mats Carlsson
 *
 * | ?- still_life(plain_mirror, 12, 12, 76).
 *
 */


:- module(life, [still_life/4]).

:- use_module(library(lists)).
:- use_module(library(avl)).
:- use_module(library(clpfd)).

still_life(plain, NR, NC, Sum) :-
	still(plain, NR, NC, Sum, Cells),
	labeling([down], Cells),
	draw(Cells, NC).
still_life(plain_rot, NR, NC, Sum) :-
	still(plain_rot, NR, NC, Sum, Cells),
	reverse(Cells, Rev),
	Cells = Rev,
	labeling([down], Cells),
	draw(Cells, NC).
still_life(plain_mirror, NR, NC, Sum) :-
	still(mirror, NR, NC, Sum, Cells),
	labeling([down], Cells),
	draw(Cells, NC).

still(mirror, NR, NC, Sum, AllCells) :- !,
	NRhalf is (NR+1)>>1,
	Ncells is NRhalf*NC,
	length(Cells, Ncells),
	domain(Cells, 0, 1),
	Ncopies is (NR>>1)*NC,
	mirror_cells(Ncopies, NC, Cells, AllCells, []),
	mirror_coeffs(Ncopies, Ncells, Coeffs, []),
	scalar_product(Coeffs, Cells, #=, Sum),
	tag_by_coords(Cells, 0, NC, KL),
	ord_list_to_avl(KL, Avl),
	(   NR/\1 =:= 0 ->
	    NR1 is NRhalf-1,
	    NR2 is NRhalf-2,
	    still_rows(-1, NR2, NC, Avl, best, Tuples, Tuples1),
	    still_even_mid_row(-1, NC, NR1, Avl, best, Tuples1, [])
	;   NR1 is NRhalf-1,
	    NR2 is NRhalf-2,
	    still_rows(-1, NR2, NC, Avl, best, Tuples, Tuples1),
	    still_odd_mid_row(-1, NC, NR1, Avl, best, Tuples1, [])
	),
	% valid3x3_table(best, Ext),
	% table(Tuples, Ext),
	cell_constraint(Tuples),
	% symmetries
	NC1 is NC-1,
	part(KL, 0, NRhalf, 0, 1, Left),
	part(KL, 0, NRhalf, NC1, NC, Right),
	lex_chain([Right,Left]).
still(_, NR, NC, Sum, Cells) :-
	Ncells is NR*NC,
	length(Cells, Ncells),
	domain(Cells, 0, 1),
	sum(Cells, #=, Sum),
	tag_by_coords(Cells, 0, NC, KL),
	ord_list_to_avl(KL, Avl),
	still_rows(-1, NR, NC, Avl, best, Tuples, []),
	% valid3x3_table(best, Ext),
	% table(Tuples, Ext),
	cell_constraint(Tuples),
	% symmetries
	NR1 is NR-1,
	NC1 is NC-1,
	part(KL, 0, 1, 0, NC,  Upper),
	part(KL, NR1, NR, 0, NC, Lower),
	part(KL, 0, NR, 0, 1, Left),
	part(KL, 0, NR, NC1, NC, Right),
	lex_chain([Lower,Upper]),
	lex_chain([Right,Left]).

mirror_cells(0, _, Cells, S0, S) :-
	append(Cells, S, S0).
mirror_cells(N1, NC, Cells, S0, S) :-
	length(Row, NC),
	append(Row, Tail, Cells),
	append(Row, S1, S0),
	N2 is N1-NC,
	mirror_cells(N2, NC, Tail, S1, S2),
	append(Row, S, S2).

cell_constraint([]).
cell_constraint([T|Ts]) :-
	var_order(best, [A,B,C,D,E,F,G,H,I], T),
	S in {0,1,2,4,5,6,12,13},
	scalar_product([10,1,1,1,1,1,1,1,1], [E,A,B,C,D,F,G,H,I], #=, S, [consistency(domain)]),
	cell_constraint(Ts).

mirror_coeffs(0, 0) --> !.
mirror_coeffs(0, J1) --> !, [1],
	{J2 is J1-1},
	mirror_coeffs(0, J2).
mirror_coeffs(I1, J1) --> [2],
	{I2 is I1-1},
	{J2 is J1-1},
	mirror_coeffs(I2, J2).

part([], _, _, _, _, []).
part([(R,C)-X|KL], Rmin, Rmax, Cmin, Cmax, L1) :-
	(   R>=Rmin, R<Rmax, C>=Cmin, C<Cmax -> L1=[X|L1b]
	;   L1=L1b
	),
	part(KL, Rmin, Rmax, Cmin, Cmax, L1b).

tag_by_coords([], _, _, []).
tag_by_coords([C|Cells], I, NC, [(Row,Col)-C|KL]) :-
	Row is I//NC,
	Col is I mod NC,
	J is I+1,
	tag_by_coords(Cells, J, NC, KL).

still_rows(I, NR, _, _, _Order) -->
	{I>NR}, !.
still_rows(I, NR, NC, Avl, Order) -->
	still_cells(-1, NC, I, Avl, Order),
	{J is I+1},
	still_rows(J, NR, NC, Avl, Order).

still_cells(Col, NC, _, _, _Order) -->
	{Col>NC}, !.
still_cells(Col, NC, Row, Avl, Order) --> [Tuple],
	{Up  is Row-1},
	{Down is Row+1},
	{Left is Col-1},
	{Right is Col+1},
	{elts([(Up,Left),(Up,Col),(Up,Right),(Row,Left),(Row,Col),(Row,Right),(Down,Left),(Down,Col),(Down,Right)], Avl, Tuple0, [])},
	{var_order(Order, Tuple0, Tuple)},
	{Col1 is Col+1},
	still_cells(Col1, NC, Row, Avl, Order).

still_even_mid_row(Col, NC, _, _, _Order) -->
	{Col>NC}, !.
still_even_mid_row(Col, NC, Row, Avl, Order) --> [Tuple],
	{Up  is Row-1},
	{Left is Col-1},
	{Right is Col+1},
	{elts([(Up,Left),(Up,Col),(Up,Right),(Row,Left),(Row,Col),(Row,Right),(Row,Left),(Row,Col),(Row,Right)], Avl, Tuple0, [])},
	{var_order(Order, Tuple0, Tuple)},
	{Col1 is Col+1},
	still_even_mid_row(Col1, NC, Row, Avl, Order).

still_odd_mid_row(Col, NC, _, _, _Order) -->
	{Col>NC}, !.
still_odd_mid_row(Col, NC, Row, Avl, Order) --> [Tuple],
	{Up  is Row-1},
	{Left is Col-1},
	{Right is Col+1},
	{elts([(Up,Left),(Up,Col),(Up,Right),(Row,Left),(Row,Col),(Row,Right),(Up,Left),(Up,Col),(Up,Right)], Avl, Tuple0, [])},
	{var_order(Order, Tuple0, Tuple)},
	{Col1 is Col+1},
	still_odd_mid_row(Col1, NC, Row, Avl, Order).

elts([], _) --> [].
elts([Key|Keys], Avl) --> [Elt],
	{getarr(Key, Avl, Elt)},
	elts(Keys, Avl).

getarr(Key, Avl, Val) :-
	avl_fetch(Key, Avl, Val), !.
getarr(_, _, 0).

var_order(best,			% 56/101, WINNER
	  [A,B,C,D,E,F,G,H,I],
	  [A,B,C,G,H,I,D,E,F]).

draw(Cells, NC) :-
	format('+~*c+\n', [NC,0'-]),
	(   fromto(Cells,S0,S,[]),
	    param(NC)
	do  (   for(_,1,NC),
		fromto(S0,[C|S1],S1,S),
		fromto(String,[Ch|T],T,"|\n")
	    do  (C=:=0 -> Ch is " " ; Ch is "0")
	    ),
	    format([0'||String], [])
	),
	format('+~*c+\n', [NC,0'-]).

end_of_file.

valid3x3(L1) :-
	L1 = [A,B,C,D,E,F,G,H,I],
	domain(L1, 0, 1),
	S in {0,1,2,4,5,6,7,8,12,13},
	10*E + A+B+C+D+F+G+H+I #= S,
	A+B+C+D+F #< 5,
	A+B+D+G+H #< 5,
	B+C+F+H+I #< 5,
	D+F+G+H+I #< 5,
	symmetry(rot, L1, L2),
	symmetry(rot, L2, L3),
	symmetry(rot, L3, L4),
	symmetry(mirror, L1, L5),
	symmetry(mirror, L2, L6),
	symmetry(mirror, L3, L7),
	symmetry(mirror, L4, L8),
	lex_chain([L2,L1]),
	lex_chain([L3,L1]),
	lex_chain([L4,L1]),
	lex_chain([L5,L1]),
	lex_chain([L6,L1]),
	lex_chain([L7,L1]),
	lex_chain([L8,L1]).

symmetry(rot,
	 [A,B,C,D,E,F,G,H,I],
	 [G,D,A,H,E,B,I,F,C]).
symmetry(mirror,
	 [A,B,C,D,E,F,G,H,I],
	 [C,B,A,F,E,D,I,H,G]).

symmetry_of(Pat1, Pat5) :-
	(   Pat1 = Pat4
	;   symmetry(rot, Pat1, Pat2),
	    (   Pat2 = Pat4
	    ;   symmetry(rot, Pat2, Pat3),
		(   Pat3 = Pat4
		;   symmetry(rot, Pat3, Pat4)
		)
	    )
	),
	(   Pat4 = Pat5
	;   symmetry(mirror, Pat4, Pat5)
	).

valid3x3_table(Order, T2) :-	% 259 tuples
	findall(L, valid3x3_variant(Order,L), T1),
	sort(T1, T2).

valid3x3_variant(Order, L3) :-
	valid3x3(L1),
	labeling([], L1),
	symmetry_of(L1, L2),
	var_order(Order, L2, L3).
