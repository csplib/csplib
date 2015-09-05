% 
% Car sequencing in MiniZinc.
% 
% This is based on the OPL3 model car.mod.
%
% Compare with the Comet model
% http://www.hakank.org/comet/car.co
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
% Model modified to match CSPLib data format by Chris Mears.
%
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%
% include "globals.mzn"; 
int: numclasses;
int: numoptions;
int: numcars;
set of int: Classes = 1..numclasses;
set of int: Options = 1..numoptions;
set of int: Slots = 1..numcars;

array[Classes] of int: numberPerClass;

array[Classes,Options] of int: optionsRequired;
array[Options] of int: windowSize;
array[Options] of int: optMax;

array[Options] of int: optionNumberPerClass = [sum(j in Classes) (numberPerClass[j] * optionsRequired[j,i]) | i in Options];

% decision variables

array[Slots] of var Classes: slot;
array[Options, Slots] of var 0..1: setup;


var int: z = sum(s in Classes) (s*slot[s]);

% solve minimize z;
solve :: int_search(slot, input_order, indomain_min, complete)
  satisfy;

constraint
  forall(c in Classes ) (
    sum(s in Slots ) (bool2int(slot[s] = c)) = numberPerClass[c]
  )
  /\
  forall(o in Options, s in 1..numcars - windowSize[o] + 1) (
    sum(j in s..s + windowSize[o]- 1) (setup[o,j]) <= optMax[o]
  )
  /\
  forall(o in Options, s in Slots ) (
    setup[o,s] = optionsRequired[slot[s],o]
  )
  /\
  forall(o in Options, i in 1..optionNumberPerClass[o]) (
    sum(s in 1..(numcars - i * windowSize[o])) (setup[o,s]) >=
           (optionNumberPerClass[o] - i * optMax[o])
  )
;

% for solve satisfy
% constraint z = 82;


output [
 "z: " ++ show(z) ++ "\n" ++
 "slot: " ++ show(slot) ++ "\n"
] ++
[
  if j = 1 then "\n" else " " endif ++ 
    show(setup[i,j])
  | i in Options, j in Slots
];
