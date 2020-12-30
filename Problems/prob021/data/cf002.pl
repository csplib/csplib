%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ECLiPSe module for solving Crossfigure Puzzle #2, from Thinks.com.
% See http://thinks.com/crosswords/number/xfig002.htm.
%
% This module written by Warwick Harvey, IC-Parc, wh@icparc.ic.ac.uk.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cf002).

:- export cf002/1.

:- lib(fd).
:- use_module(crossfig).

%
% cf002(M):
%	Top-level goal for solving the puzzle.
%

cf002(M) :-
	constrain(M, Vars),
	labeling(Vars),
	print_matrix(M).


constrain(M, MVars) :-
	dim(M, [9, 9]),
	dim(T, [9, 9]),


	% Set up the constraints between the matrix elements and the
	% clue numbers.

	across(M, T, A1, 2, 1, 1),
	across(M, T, A3, 3, 1, 4),
	across(M, T, A5, 2, 1, 8),
	across(M, T, A7, 4, 2, 1),
	across(M, T, A8, 4, 2, 6),
	across(M, T, A9, 3, 3, 4),
	across(M, T, A11, 3, 4, 1),
	across(M, T, A13, 3, 4, 7),
	across(M, T, A15, 2, 5, 3),
	across(M, T, A16, 2, 5, 6),
	across(M, T, A17, 3, 6, 1),
	across(M, T, A20, 3, 6, 7),
	across(M, T, A22, 3, 7, 4),
	across(M, T, A24, 4, 8, 1),
	across(M, T, A25, 4, 8, 6),
	across(M, T, A27, 2, 9, 1),
	across(M, T, A28, 3, 9, 4),
	across(M, T, A29, 2, 9, 8),

	down(M, T, D1, 2, 1, 1),
	down(M, T, D2, 4, 1, 2),
	down(M, T, D3, 3, 1, 4),
	down(M, T, D4, 3, 1, 6),
	down(M, T, D5, 4, 1, 8),
	down(M, T, D6, 2, 1, 9),
	down(M, T, D10, 2, 3, 5),
	down(M, T, D11, 3, 4, 1),
	down(M, T, D12, 3, 4, 3),
	down(M, T, D13, 3, 4, 7),
	down(M, T, D14, 3, 4, 9),
	down(M, T, D18, 4, 6, 2),
	down(M, T, D19, 2, 6, 5),
	down(M, T, D21, 4, 6, 8),
	down(M, T, D22, 3, 7, 4),
	down(M, T, D23, 3, 7, 6),
	down(M, T, D24, 2, 8, 1),
	down(M, T, D26, 2, 8, 9),

	init_matrix(M, T, MVars),

	% Make a nice graphical display of the search if the user is
	% using TkECLiPSe.
	make_display_matrix(M, matrix),


	% Set up the clue constraints.

	/*
	Across

	 1 A prime number
	 3 27 across times three
	 5 6 down plus twenty-five
	 7 5 down minus 821
	 8 2 down plus 1046
	 9 24 down times four
	11 21 down divided by six
	13 21 down divided by three
	15 13 down divided by eleven
	16 12 down divided by seven
	17 9 across minus sixteen
	20 A square number
	22 20 across plus three
	24 8 across minus 234
	25 24 across minus 122
	27 22 across divided by six
	28 22 down plus nineteen
	29 26 down minus twenty 
	*/

	prime(A1),
	A3 #= 3 * A27,
	A5 #= D6 + 25,
	A7 #= D5 - 821,
	A8 #= D2 + 1046,
	A9 #= 4 * D24,
	A11 #= D21 / 6,
	A13 #= D21 / 3,
	A15 #= D13 / 11,
	A16 #= D12 / 7,
	A17 #= A9 - 16,
	square(A20),
	A22 #= A20 + 3,
	A24 #= A8 - 234,
	A25 #= A24 - 122,
	A27 #= A22 / 6,
	A28 #= D22 + 19,
	A29 #= D26 - 20,

	/*
	Down

	 1 Two times 24 down
	 2 23 down times four
	 3 21 down divided by twelve
	 4 28 across plus 161
	 5 1 across times 1 down
	 6 29 across times two
	10 13 across divided by nine
	11 3 down plus 180
	12 Seven times 27 across
	13 23 down minus 159
	14 26 down times eleven
	18 8 across plus 908
	19 10 down minus four
	21 22 across times nine
	22 22 across plus thirty-four
	23 A square number
	24 A prime number
	26 20 across divided by five 
	*/

	D1 #= 2 * D24,
	D2 #= 4 * D23,
	D3 #= D21 / 12,
	D4 #= A28 + 161,
	D5 #= A1 * D1,
	D6 #= 2 * A29,
	D10 #= A13 / 9,
	D11 #= D3 + 180,
	D12 #= 7 * A27,
	D13 #= D23 - 159,
	D14 #= 11 * D26,
	D18 #= A8 + 908,
	D19 #= D10 - 4,
	D21 #= 9 * A22,
	D22 #= A22 + 34,
	square(D23),
	prime(D24),
	D26 #= A20 / 5.

