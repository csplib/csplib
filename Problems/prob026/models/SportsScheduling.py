"""
PyCSP3 Model (see pycsp.org)

Examples:
  python SportsScheduling.py
  python SportsScheduling.py -data=10
  python SportsScheduling.py -data=10 -variant=dummy
"""

from pycsp3 import *

nTeams = data or 8
nWeeks, nPeriods, nMatches = nTeams - 1, nTeams // 2, (nTeams - 1) * nTeams // 2


def match_number(t1, t2):
    return nMatches - ((nTeams - t1) * (nTeams - t1 - 1)) // 2 + (t2 - t1 - 1)


table = {(t1, t2, match_number(t1, t2)) for t1, t2 in combinations(range(nTeams), 2)}

# m[w][p] is the number of the match at week w and period p
m = VarArray(size=[nWeeks, nPeriods], dom=range(nMatches))

# x[w][p] is the first team for the match at week w and period p
x = VarArray(size=[nWeeks, nPeriods], dom=range(nTeams))

# y[w][p] is the second team for the match at week w and period p
y = VarArray(size=[nWeeks, nPeriods], dom=range(nTeams))

satisfy(
    # all matches are different (no team can play twice against another team)
    AllDifferent(m),

    # linking variables through ternary table constraints
    [(x[w][p], y[w][p], m[w][p]) in table for w in range(nWeeks) for p in range(nPeriods)],

    # each week, all teams are different (each team plays each week)
    [AllDifferent(x[w] + y[w]) for w in range(nWeeks)],

    # each team plays at most two times in each period
    [Cardinality(x[:, p] + y[:, p], occurrences={t: range(1, 3) for t in range(nTeams)}) for p in range(nPeriods)],

    # tag(symmetry-breaking)
    [
        # the match '0 versus t' (with t strictly greater than 0) appears at week t-1
        [Count(m[w], value=match_number(0, w + 1)) == 1 for w in range(nWeeks)],

        # the first week is set : 0 vs 1, 2 vs 3, 4 vs 5, etc.
        [m[0][p] == match_number(2 * p, 2 * p + 1) for p in range(nPeriods)]
    ]
)

if variant("dummy"):
    # xd[p] is the first team for the dummy match of period p  tag(dummy-week)
    xd = VarArray(size=nPeriods, dom=range(nTeams))

    # yd[p] is the second team for the dummy match of period p  tag(dummy-week)
    yd = VarArray(size=nPeriods, dom=range(nTeams))

    satisfy(
        # handling dummy week (variables and constraints)  tag(dummy-week)
        [
            # all teams are different in the dummy week
            AllDifferent(xd + yd),

            # each team plays two times in each period
            [Cardinality(x[:, p] + y[:, p] + [xd[p], yd[p]], occurrences={t: 2 for t in range(nTeams)}) for p in range(nPeriods)],

            # tag(symmetry-breaking)
            [xd[p] < yd[p] for p in range(nPeriods)]
        ]
    )
