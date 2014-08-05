/*

  Magic Hexagon in SICStus Prolog.

  Prob023: Magic Hexagon
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
  http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/magic_hexagon.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        LD = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S],
        domain(LD,1,19),

        all_distinct(LD),

        A + B + C #=  38,
        D + E + F + G #=  38,
        H + I + J + K + L #=  38, 
        M + N + O + P #=  38, 
        Q + R + S #=  38, 
        A + D + H #=  38, 
        B + E + I + M #=  38, 
        C + F + J + N + Q #=  38, 
        G + K + O + R #=  38, 
        L + P + S #=  38, 
        C + G + L #=  38, 
        B + F + K + P #=  38, 
        A + E + J + O + S #=  38, 
        D + I + N + R #=  38, 
        H + M + Q #=  38, 
        
        A #< C,
        A #< H,
        A #< L,
        A #< Q,
        A #< S,
        C #< H,
        
        labeling([ff,bisect,up], LD),

        write(LD),nl,
        fd_statistics.
