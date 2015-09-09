%
% A Golomb ruler is a set of integers (marks) a(1) < ...  < a(n) such
% that all the differences a(i)-a(j) (i > j) are distinct.  Clearly we
% may assume a(1)=0.  Then a(n) is the length of the Golomb ruler.  For
% a given number of marks, n, we are interested in finding the shortest
% Golomb rulers.  Such rulers are called optimal. 
%
% Currently (1999), optimal rulers are known up to n=21.
% See http://www.research.ibm.com/people/s/shearer/grule.html
%
% ECLiPSe solution by Joachim Schimpf, IC-Parc. The code is inspired
% by Jean-Francois Puget and Michel Leconte's ILOG solver solution.
%
%  N    Opt     Backtr  Backtr  Time(s)
%               to opt   total   total
%  5    11           0       0      0.0
%  6    17           0       3      0.0
%  7    25           7      24      0.4
%  8    34          45     186      3.8
%  9    44         309    1013     33.4
% 10    55        2797    6008    287
% 11    72       15597   88764   6500
% 12    85      487865  763328  75300
%

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
:- import alldifferent/1 from ic_global.

golomb(N, Xs) :-
        length(Xs, N),                          % model
        NN is 2^(N-1)-1,
        Xs :: [0..NN],
        once append([0|_], [Xn], Xs),

        ordered(<, Xs),
        distances(Xs, Diffs),
        Diffs::[1..NN],
        alldifferent(Diffs),
        once append([D1|_], [Dn], Diffs),
        D1 #< Dn,

        bb_min(labeling(Xs), Xn, _default).    % search


    distances([], []).
    distances([X|Ys], D0) :-
        distances(X, Ys, D0, D1),
        distances(Ys, D1).

    distances(_, [], D, D).
    distances(X, [Y|Ys], [Diff|D1], D0) :-
        Diff #= Y-X,
        distances(X, Ys, D1, D0).

