:- use_module(library(lists)).

% Schur's Lemma (CSPLib prob015)
% Colour the integers 1..N with C colours so that no colour class contains
% three integers x, y, z (not necessarily distinct) with x + y = z.

% schur(+N, +C, -Colours)
schur(N, C, Colours) :-
    N >= 1,
    numlist(1, C, Palette),             % the available colours 1..C
    % symmetry break: integer 1 is fixed to colour 1, then colour 2,3,...,N
    colour(2, N, Palette, [1-1], Colours).

% colour(+I, +N, +Palette, +Assigned, -Colours)
% Assigned holds Integer-Colour pairs for the integers already coloured.
colour(I, N, _, Assigned, Colours) :-
    I > N,                              % every integer has a colour
    reverse(Assigned, Pairs),
    pairs_values(Pairs, Colours).
colour(I, N, Palette, Assigned, Colours) :-
    I =< N,
    member(Col, Palette),               % try a colour for integer I
    safe(I, Col, Assigned),             % colouring I with Col stays sum-free
    I1 is I + 1,
    colour(I1, N, Palette, [I-Col|Assigned], Colours).

% safe(+I, +Col, +Assigned): giving integer I the colour Col creates no
% monochromatic x + y = I among the already-coloured integers.
safe(I, Col, Assigned) :-
    \+ ( between(1, I, X),              % X is one summand of I
         Y is I - X, Y >= X,            % Y the other; Y >= X avoids checking twice
         memberchk(X-Col, Assigned),    % X already carries colour Col
         memberchk(Y-Col, Assigned) ).  % Y already carries colour Col
