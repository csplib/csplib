/*

  Killer Sudoku in ECLiPSe.

  http://en.wikipedia.org/wiki/Killer_Sudoku
  """
  Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or 
  samunamupure) is a puzzle that combines elements of sudoku and kakuro. 
  Despite the name, the simpler killer sudokus can be easier to solve 
  than regular sudokus, depending on the solver's skill at mental arithmetic; 
  the hardest ones, however, can take hours to crack.

  ...

  The objective is to fill the grid with numbers from 1 to 9 in a way that 
  the following conditions are met:

    * Each row, column, and nonet contains each number exactly once.
    * The sum of all numbers in a cage must match the small number printed 
      in its corner.
    * No number appears more than once in a cage. (This is the standard rule 
      for killer sudokus, and implies that no cage can include more 
      than 9 cells.)

  In 'Killer X', an additional rule is that each of the long diagonals 
  contains each number once.
  """

  Here we solve the problem from the Wikipedia page, also shown here
  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

  The output is:
    2 1 5 6 4 7 3 9 8
    3 6 8 9 5 2 1 7 4
    7 9 4 3 8 1 6 5 2
    5 8 6 2 7 4 9 3 1
    1 4 2 5 9 3 8 6 7
    9 7 3 8 1 6 4 2 5
    8 2 1 7 3 9 5 4 6
    6 5 9 4 2 8 7 1 3
    4 3 7 1 6 5 2 8 9


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/killer_sudoku.co
  * MiniZinc: http://www.hakank.org/minizinc/killer_sudoku.mzn
  * SICStus: http://www.hakank.org/sicstus/killer_sudoku.pl

  These models uses the same principle that is used here:
  * ECLiPSe : http://www.hakank.org/eclipse/kenken2.ecl
  * ECLiPSe : http://www.hakank.org/eclipse/kakuro.ecl
  * SICStus : http://www.hakank.org/eclipse/sudoku_gcc.pl


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

  Model simplified by Joachim Schimpf

*/

% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/

:-lib(ic).
:-lib(ic_global).
:-import alldifferent/1 from ic_global.


go :- 
        problem(1,Hints),
        killer_sudoku(Hints,X,Backtracks),
        ( foreacharg(Row,X) do
          writeln(Row)
        ),
        nl,
        writeln(backtracks:Backtracks).


killer_sudoku(Problem,X,Backtracks) :-
        
        dim(X,[9,9]),
        X :: 1..9,

        % rows and columns
        ( for(I,1,9), param(X) do
          alldifferent(X[I,1..9]),
          alldifferent(X[1..9,I])
        ),

        % 3x3 squares
        ( multifor([I,J],1,9,3), param(X) do
            ( multifor([K,L],0,2), foreach(Field,SubSquare), param(X,I,J) do
                Field is X[I+K,J+L]
            ),
            alldifferent(SubSquare)
        ),

        % cages and sum hints
        ( foreach([Sum|Indices],Problem), param(X) do 
            ( foreach(Index,Indices), foreach(Field,Cage), param(X) do
                subscript(X, Index, Field)
            ),
            sum(Cage)#=Sum,
            alldifferent(Cage)
        ),

        array_flat(2, X, Vars),
        search(Vars,0,first_fail,indomain_min,complete,[backtrack(Backtracks)]).



problem(1, 
        [% The hints:
         %  [Sum, [list of indices in X]]
            [ 3, [1,1], [1,2]],
            [15, [1,3], [1,4], [1,5]],
            [22, [1,6], [2,5], [2,6], [3,5]],
            [ 4, [1,7], [2,7]],
            [16, [1,8], [2,8]],
            [15, [1,9], [2,9], [3,9], [4,9]],
            [25, [2,1], [2,2], [3,1], [3,2]],
            [17, [2,3], [2,4]],
            [ 9, [3,3], [3,4], [4,4]],
            [ 8, [3,6], [4,6],[5,6]],
            [20, [3,7], [3,8],[4,7]],
            [ 6, [4,1], [5,1]],
            [14, [4,2], [4,3]],
            [17, [4,5], [5,5],[6,5]],
            [17, [4,8], [5,7],[5,8]],
            [13, [5,2], [5,3],[6,2]],
            [20, [5,4], [6,4],[7,4]],
            [12, [5,9], [6,9]],
            [27, [6,1], [7,1],[8,1],[9,1]],
            [ 6, [6,3], [7,2],[7,3]],
            [20, [6,6], [7,6], [7,7]],
            [ 6, [6,7], [6,8]],
            [10, [7,5], [8,4],[8,5],[9,4]],
            [14, [7,8], [7,9],[8,8],[8,9]],
            [ 8, [8,2], [9,2]],
            [16, [8,3], [9,3]],
            [15, [8,6], [8,7]],
            [13, [9,5], [9,6],[9,7]],
            [17, [9,8], [9,9]]
        ]).

