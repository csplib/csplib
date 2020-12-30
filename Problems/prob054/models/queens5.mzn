% 
% n queens problem in MiniZinc.
% 
% Using alldifferent. 
%
 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%
include "globals.mzn";

int: n;

array[1..n] of var 1..n: queens;

solve :: int_search(
        queens, 
        first_fail,
        indomain_median,
        complete
       )
     satisfy;

% solve satisfy;
constraint all_different(queens); %  :: domain;
constraint all_different([queens[i]+i | i in 1..n]) :: domain;
constraint all_different([queens[i]-i | i in 1..n]) :: domain;

output 
[
  "queens: " ++ show(queens)
] ++ 
% [
%   if j = 1 then "\n" else "" endif ++
%      if fix(queens[i]) = j then          
%         show_int(2,j)
%      else
%         "__"
%      endif
%   | i in 1..n, j in 1..n
% ] ++
["\n"]
;

% data
n = 8;