% prob008.mzn: Vessel Loading
% Problem details available at
% http://www.csplib.org/Problems/prob008/

% Author: Frej Knutar Lewander

int: deck_width; % width of deck
int: deck_length; % height of deck
int: n_containers; % number of containers
int: n_classes; % number of container classes

array[int] of int: width;
% width[c] = width of container c
array[int] of int: length;
% length[c] = length of container c
array[int] of int: class;
% class[c] = class of container c
array[int, int] of int: separation;
% separation[a, b] = the minimum allowed separation between
%                    containers of classes a and b

set of int: Containers = 1..n_containers;

array[Containers] of var 0..deck_width: Left;
% Left[c] = leftmost point of container c
array[Containers] of var 0..deck_width: Right;
% Right[c] = rightmost point of container c
array[Containers] of var 0..deck_length: Bottom;
% Bottom[c] = bottommost point of container c
array[Containers] of var 0..deck_length: Top;
% Top[c] = topmost point of container c

array[Containers] of var 1..2: orientation;
% orientation[c] = 2 if container c is turned 90 degrees, else 1.

constraint
  forall (c in Containers) (
    let {
      array[1..2] of int: ElemWidth = [width[c], length[c]];
      array[1..2] of int: ElemLength = [length[c], width[c]]
    }
    in 
      Right[c] = Left[c] + ElemWidth[orientation[c]] /\
      Top[c] = Bottom[c] + ElemLength[orientation[c]]
  )
;

constraint
  forall(c, k in Containers where c < k) (
    Left[c] >= Right[k] + separation[class[c], class[k]] \/
    Right[c] + separation[class[c], class[k]] <= Left[k] \/
    Bottom[c] >= Top[k] + separation[class[c], class[k]] \/
    Top[c] + separation[class[c], class[k]] <= Bottom[k]
  )
;

% Excessive (pretty) output
output ["┌"] ++ ["─" | t in 0..deck_width] ++ ["┐\n"] ++
       [if x == 0 then "│" else "" endif ++
        let {
          bool: l = exists([fix(Left[c])   = x | c in Containers]);
          bool: r = exists([fix(Right[c])  = x | c in Containers]);
          bool: b = exists([fix(Bottom[c]) = y | c in Containers]);
          bool: t = exists([fix(Top[c])    = y | c in Containers])
        } in
          if     l /\ r /\ b /\ t then "┼"
          elseif l /\ r /\ b      then "┬"
          elseif l /\ r /\      t then "┴"
          elseif l /\      b /\ t then "├"
          elseif      r /\ b /\ t then "┤"
          elseif l /\      b      then "┌"
          elseif l /\           t then "└"
          elseif      r /\ b      then "┐"
          elseif      r /\      t then "┘"
          elseif l \/ r           then "│"
          elseif           b \/ t then "─" 
          else " " endif ++
        if x == deck_width then "│\n" else "" endif
        | y in 0..deck_length, x in 0..deck_width] ++
        ["└"] ++ ["─" | t in 0..deck_width] ++ ["┘"];
