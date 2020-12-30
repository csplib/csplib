%
% ECLiPSe sample program
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%
% Various models for the N-Queens problem
%
% Author: Joachim Schimpf.  Use at your own risk for any purpose.
%
% Sample run:
%
%     % eclipse -f queens.ecl -e "queens(8)"
%     ....Q...
%     Q.......
%     .......Q
%     ...Q....
%     .Q......
%     ......Q.
%     ..Q.....
%     .....Q..
%     

:- lib(ic).


queens(N, Qs) :-

        % The model: Qs is a list of column numbers
        length(Qs, N),
        Qs :: 1..N,
        ( fromto(Qs,[Q1|Q2s],Q2s,[]) do
            ( foreach(Q2,Q2s), param(Q1), count(Dist,1,_) do
                Q2 #\= Q1,
                Q2-Q1 #\= Dist,
                Q1-Q2 #\= Dist
            )
        ),

        % Search heuristic for larger instances: start in the middle
        lists:middle_out(Qs, Ordered),
        search(Ordered, 0, first_fail, indomain_middle, complete, []).


% With solution printing

queens(N) :-
        queens(N, Qs),
        ( foreach(Q,Qs), param(N) do
            L is Q-1, R is N-Q,
            printf("%*cQ%*c%n", [L,0'.,R,0'.])
        ).


% Counting solutions

queens_all(N) :-
        shelf_create(count(0), Count),
        \+ (queens(N, _), shelf_inc(Count, 1), fail),
        shelf_get(Count, 1, NSol),
        printf("%d queens: %d solutions%n", [N,NSol]).

