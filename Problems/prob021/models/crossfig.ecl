%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ECLiPSe library for solving "crossfigures" puzzles.
%
% "Crossfigures" puzzles correspond to problem 21 in the CSPLib.
% See www.csplib.org for more details.
%
% Particular instances can be found at thinks.com/crosswords/xfig.htm.
%
% This module written by Warwick Harvey, IC-Parc, wh@icparc.ic.ac.uk.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(crossfig).

:- export across/6, down/6, init_matrix/3, print_matrix/1.
:- export square/1, prime/1.

:- lib(fd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The problem is modelled using an array `Matrix' to represent the puzzle
% "board".  A second array `Template' is used to indicate whether each
% element of `Matrix' should contain a digit or should be blank.  This
% information can also be used to perform some integrity checks, to help
% catch errors in the expression of a problem.
%
% The multidigit numbers used in the "clues" (1 across, 7 down, etc.) are
% set up using the predicates `across/6' and `down/6', which relate these
% numbers to the digits in `Matrix'.  Once these are all set up,
% `init_matrix/3' should be called to complete the initialisation of
% `Matrix', before the clue constraints are added.
%
% Also provided are a number of predicates which are useful for
% expressing clue constraints such as "A square number" and "A prime
% number".
%
% See one of the accompanying problem modules (cf*.pl) for an example of
% how it all works.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% across(Matrix, Template, Across, Len, Row, Col):
%	Constrains `Across' to be equal to the number represented by the
%	`Len' digits starting at position (Row, Col) of the array `Matrix'
%	and proceeding across.
%	`Template' is used for integrity checking, as well as for collecting
%	information about which elements of `Matrix' should contain digits,
%	and which should be empty.
%

across(Matrix, Template, Across, Len, Row, Col) :-
	% Constrain `Across' to be equal to the corresponding digits.
	(
		for(I, Len-1, 0, -1),
		fromto(1, Mult, NewMult, _),
		fromto(0, SumIn, SumOut, Across),
		param(Matrix, Row, Col)
	do
		Elem is Matrix[Row, Col + I],
		Elem :: [0..9],
		SumOut #= SumIn + Mult * Elem,
		NewMult is Mult * 10
	),

	% Integrity checks.
	dim(Template, [_Height, Width]),
	(
		Template[Row, Col .. Col + Len - 1] :: 1,
		( Col > 1            -> Template[Row, Col - 1] :: 0   ; true ),
		( Col + Len =< Width -> Template[Row, Col + Len] :: 0 ; true )
	->
		true
	;
		printf(error, "Crossfigure integrity violation adding "
			"an across figure of length %d,%n"
			"starting at (%d, %d)%n",
			[Len, Row, Col]),
		abort
	).


%
% down(Matrix, Template, Down, Len, Row, Col):
%	Constrains `Down' to be equal to the number represented by the
%	`Len' digits starting at position (Row, Col) of the array `Matrix'
%	and proceeding down.
%	`Template' is used for integrity checking, as well as for collecting
%	information about which elements of `Matrix' should contain digits,
%	and which should be empty.
%

down(Matrix, Template, Down, Len, Row, Col) :-
	% Constrain `Down' to be equal to the corresponding digits.
	(
		for(I, Len-1, 0, -1),
		fromto(1, Mult, NewMult, _),
		fromto(0, SumIn, SumOut, Down),
		param(Matrix, Row, Col)
	do
		Elem is Matrix[Row + I, Col],
		Elem :: [0..9],
		SumOut #= SumIn + Mult * Elem,
		NewMult is Mult * 10
	),

	% Integrity checks.
	dim(Template, [Height, _Width]),
	(
		Template[Row .. Row + Len - 1, Col] :: 1,
		( Row > 1             -> Template[Row - 1, Col] :: 0   ; true ),
		( Row + Len =< Height -> Template[Row + Len, Col] :: 0 ; true )
	->
		true
	;
		printf(error, "Crossfigure integrity violation adding "
			"a down figure of length %d,%n"
			"starting at (%d, %d)%n",
			[Len, Row, Col]),
		abort
	).


%
% init_matrix(Matrix, Template, Vars):
%	Finishes the initialisation of `Matrix', returning a list of all
%	the variables in it in `Vars'.
%	`Template' is used to determine which elements of `Matrix' should be
%	variables, and which ones should be blank.  Blank elements are
%	filled with a ` '.
%

init_matrix(Matrix, Template, Vars) :-
	dim(Matrix, [Row, Col]),
	(
		for(I, 1, Row),
		fromto([], Vars1, Vars4, Vars),
		param(Matrix, Template, Col)
	do
		(
			for(J, 1, Col),
			fromto(Vars1, Vars2, Vars3, Vars4),
			param(Matrix, Template, I)
		do
			T is Template[I, J],
			Elem is Matrix[I, J],
			( var(T) ->
				T = 0
			;
				true
			),
			( T = 0 ->
				Elem = ' ',
				Vars3 = Vars2
			;
				Vars3 = [Elem | Vars2]
			)
		)
	).


%
% print_matrix(Matrix):
%	Prints `Matrix' in a readable format.
%

print_matrix(Matrix) :-
	nl,
	(
		foreacharg(Row, Matrix)
	do
		write(' '),
		(
			foreacharg(Elem, Row)
		do
			write(Elem)
		),
		nl
	).


%-------- Useful constraints for crossfigure puzzles --------%

%
% square(N):
%	Constrains N to be a square number.
%

square(N) :-
	N #= T * T.


%
% prime(N):
%	Delays until N is ground, and then succeeds if and only if it is
%	prime.
%

prime(N) :-
	( nonvar(N) ->
		is_prime_2(2, N)
	;
		suspend(prime(N), 2, N->inst)
	).

is_prime_2(Q, N) :-
	N mod Q =\= 0,
	( Q * Q < N ->
		Q1 is Q + 1,
		is_prime_2(Q1, N)
	;
		true
	).

