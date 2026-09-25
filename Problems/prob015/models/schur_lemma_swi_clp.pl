:- use_module(library(clpfd)).

% Schur's Lemma (CSPLib prob015)
% Colour the integers 1..N with C colours so that no colour class contains
% three integers x, y, z (not necessarily distinct) with x + y = z.

% schur(+N, +C, -Colours)
schur(N, C, Colours) :-
    length(Colours, N),                 % Colours[i] = colour of integer i
    Colours ins 1..C,
    Colours = [1|_],                    % symmetry break: integer 1 gets colour 1
    triples(N, Triples),                % all (x, y, z) with x =< y and z = x+y =< N
    maplist(sum_free(Colours), Triples),
    label(Colours).

% triples(+N, -Triples): every triple X-Y-Z with X =< Y and Z = X+Y =< N
triples(N, Triples) :-
    findall(X-Y-Z,
            ( between(1, N, X),
              between(X, N, Y),
              Z is X + Y, Z =< N ),
            Triples).

% sum_free(+Colours, +Triple): integers x, y, z must not all share one colour
sum_free(Colours, X-Y-Z) :-
    nth1(X, Colours, CX),
    nth1(Y, Colours, CY),
    nth1(Z, Colours, CZ),
    (CX #\= CY) #\/ (CY #\= CZ).         % at least one pair must differ in colour
