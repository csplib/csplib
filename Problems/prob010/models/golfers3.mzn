% golfers.mzn
% vim: ft=zinc ts=4 sw=4 et tw=0
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Mon Oct 29 13:56:25 EST 2007
%
% The social golfers problem, see
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob001/data.txt
%
% A club has a number of golfers that play rounds in groups (the number of
% golfers is a multiple of the number of groups).  Each round, a golfer
% plays with a group of different people, such that the same pair of golfers
% never play together twice.

include "globals.mzn";

int: n_groups;                          % The number of groups.
int: n_per_group;                       % The size of each group.
int: n_rounds;                          % The number of rounds.

int: n_golfers = n_groups * n_per_group;

set of int: groups = 1..n_groups;
set of int: group = 1..n_per_group;
set of int: rounds = 1..n_rounds;
set of int: golfers = 1..n_golfers;

array [rounds, groups] of var set of golfers: round_group_golfers;

    % Each group has to have the right size.
    %
constraint
    forall (r in rounds, g in groups) (
        card(round_group_golfers[r, g]) = n_per_group
    );

    % Each group in each round has to be disjoint.
    %
constraint
    forall (r in rounds) (
        all_disjoint (g in groups) (round_group_golfers[r, g])
    );

    % Symmetry breaking.
    %
% constraint
%     forall (r in rounds, g in groups where g < n_groups) (
%         round_group_golfers[r, g] < round_group_golfers[r, g + 1]
%     );

    % Each pair may play together at most once.
    %
constraint
    forall (a, b in golfers where a < b) (
        sum (r in rounds, g in groups) (
            bool2int({a, b} subset round_group_golfers[r, g])
        )
    <=
        1
    );

solve satisfy;

output [    ( if   g = 1
              then "\nround " ++ show(r) ++ ":   "
              else "   "
              endif
            ) ++
            show(round_group_golfers[r, g])
       |    r in rounds, g in groups
       ];


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
