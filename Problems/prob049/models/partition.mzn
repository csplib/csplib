%-----------------------------------------------------------------------------%
% Partitioning problem
%
% Guido Tack
% 05/2009
%
%
% Partition 2*n numbers into two groups, each of size n, such that
% their sums are equal and the sums of their squares are equal.
%

include "globals.mzn";

%-----------------------------------------------------------------------------%
% Instance
%-----------------------------------------------------------------------------%

n = 32;

%-----------------------------------------------------------------------------%
% Model
%-----------------------------------------------------------------------------%

int: n;

array[1..n] of var 1..2*n: x;
array[1..n] of var 1..2*n: y;

constraint true
    %   Break symmetries by ordering numbers in each group
    /\  forall (i in 2..n) (x[i-1] < x[i] /\ y[i-1] < y[i])
    %   Break symmetries by ordering the groups
    /\  x[1] < y[1]
    
    %   Partition the numbers
    /\  (alldifferent(x++y)) :: bounds
    
    %   The sums are equal
    /\  sum (x) = 2*n*(2*n+1) div 4
    /\  sum (y) = 2*n*(2*n+1) div 4
    
    %   The sums of the squares are equal
    /\  let {
            array[1..n] of var 1..4*n*n: sx,
            array[1..n] of var 1..4*n*n: sy
        } in
        forall (i in 1..n) (sx[i]=x[i]*x[i] /\ sy[i] = y[i]*y[i])
    /\  sum (sx) = 2*n*(2*n+1)*(4*n+1) div 12
    /\  sum (sy) = 2*n*(2*n+1)*(4*n+1) div 12
;

solve ::int_search(x++y,first_fail,indomain_min,complete) satisfy;

output
    ["x = ",show(x),"\n","y = ",show(y),"\n",
     "sum = ",show(2*n*(2*n+1) div 4),"\n",
     "sum of squares = ", show(2*n*(2*n+1)*(4*n+1) div 12), "\n"
     ];

