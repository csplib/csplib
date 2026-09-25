% n_queens2(+N, -Solution): Solution lists the row of the queen in each column
n_queens2(N, Solution) :-
    numlist(1, N, AllRows),
    numlist(1, N, AllCols),
    place_queens(AllCols, AllRows, [], Solution).

% Base case: No more columns/rows available. Terminate the solution list with [].
% place_queens(+Cols, +AvailableRows, +PrevQueens, -Solution): place one queen per column
place_queens([], [], _, []).
place_queens([Col|RestCols], AvailableRows, PrevQueens, [Row|RestSolution]) :-
    % 1. Select a row from available rows and store the remaining in RestRows
    select(Row, AvailableRows, RestRows),
    % 2. Check immediately if this new position is safe from previous queens
    check_position(Col, Row, PrevQueens),
    % 3. If safe, store the queen
    NewPrevQueens = [dame(Col, Row) | PrevQueens],
    %Recursion: Process next queen
    place_queens(RestCols, RestRows, NewPrevQueens, RestSolution).

% Check if the new queen is safe from the list of already placed queens
% check_position(+Col, +Row, +PrevQueens): the queen at (Col,Row) attacks no previous queen
check_position(_, _, []).
check_position(Col, Row, [dame(Col2, Row2) | OtherQueens]) :-
    diagonal_desc(Col, Row, Col2, Row2),
    diagonal_asc(Col, Row, Col2, Row2),
    check_position(Col, Row, OtherQueens).

% Diagonal checks: return true if not on the same diagonal
% diagonal_desc(+C1, +R1, +C2, +R2): the two queens are not on the same descending diagonal
diagonal_desc(C1, R1, C2, R2) :- C1 - R1 =\= C2 - R2.
% diagonal_asc(+C1, +R1, +C2, +R2): the two queens are not on the same ascending diagonal
diagonal_asc(C1, R1, C2, R2) :- C1 + R1 =\= C2 + R2.
