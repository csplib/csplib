% 
% Magic Hexagon in MiniZinc.
%
% Prob023: Magic Hexagon
% http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
% http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/
% 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: N = 1..19;
var N: a;
var N: b;
var N: c;
var N: d;
var N: e;
var N: f;
var N: g;
var N: h;
var N: i;
var N: j;
var N: k;
var N: l;
var N: m;
var N: n;
var N: o;
var N: p;
var N: q;
var N: r;
var N: s;

array[N] of var N: LD = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s];

solve satisfy;

constraint
  all_different(LD) /\

  a + b + c =  38 /\
  d + e + f + g =  38 /\
  h + i + j + k + l =  38 /\ 
  m + n + o + p =  38 /\ 
  q + r + s =  38 /\ 
  a + d + h =  38 /\ 
  b + e + i + m =  38 /\ 
  c + f + j + n + q =  38 /\ 
  g + k + o + r =  38 /\ 
  l + p + s =  38 /\ 
  c + g + l =  38 /\ 
  b + f + k + p =  38 /\ 
  a + e + j + o + s =  38 /\ 
  d + i + n + r =  38 /\ 
  h + m + q =  38 /\ 

  a < c /\
  a < h /\
  a < l /\
  a < q /\
  a < s /\
  c < h
;

output
[
  show(LD)
];