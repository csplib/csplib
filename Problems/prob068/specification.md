---
Title:    Traveling Tournament Problem with Predefined Venues (TTPPV)
Proposer: Gilles Pesant
Category: Scheduling and related problems
---

The TTPPV was introduced by [MeloUR09] and consists of finding an optimal compact single round robin schedule for a sport event. Given a set of $n$ teams, each team has to play against every other team exactly once. In each round, a team plays either at home or away, however no team can play more than three consecutive times at home or away. The sum of the traveling distance of each team has to be minimized. The particularity of this problem resides on the venue of each game that is predefined, i.e. if team $a$ plays against $b$ it is already known whether the game is going to be held at $a$'s home or at $b$'s home. A TTPPV instance is said to be balanced if the number of home games and the number of away games differ by at most one for each team; otherwise it is referred to as non-balanced or random. Non-balanced instances may be infeasible.

Here is a feasible solution for 8 teams, with each row describing the schedule for an individual team and giving its travel distance: 
```
  1    7   @4   @5   @6    2   @3       team travel distance: 14
 @0    5    7   @6   @4    3   @2  	team travel distance: 12
 @3    4    6    7   @5   @0    1  	team travel distance: 10
  2    6    5   @4   @7   @1    0  	team travel distance: 8
  6   @2    0    3    1   @7   @5  	team travel distance: 10
 @7   @1   @3    0    2   @6    4  	team travel distance: 10
 @4   @3   @2    1    0    5   @7  	team travel distance: 10
  5   @0   @1   @2    3    4    6  	team travel distance: 6
total cost: 80
```
The "@" prefix means that the team is playing away, e.g. team 0 is playing away against team 4 in the third round.
