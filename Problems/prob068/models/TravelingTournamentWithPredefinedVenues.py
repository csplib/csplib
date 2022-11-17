"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python TravelingTournamentWithPredefinedVenues.py -data=Ttppv_circ8bbal.json -variant=a2
  python TravelingTournamentWithPredefinedVenues.py -data=Ttppv_circ8bbal.json -variant=a3
"""

from pycsp3 import *

nTeams, venues = data
nRounds = nTeams - 1
assert nTeams % 2 == 0, "an even number of teams is expected"
nConsecutiveGames = 2 if variant("a2") else 3  # used in one comment


def table(i, at_end=False):  # when at_end is True, this is for the first or last game of the ith team
    def cd(v1, v2):  # circular distance
        return min(abs(v1 - v2), nTeams - abs(v1 - v2))

    if at_end:  # note that when playing at home (whatever the opponent), the travel distance is 0
        return {(1, ANY, 0)} | {(0, j, cd(i, j)) for j in range(nTeams) if j != i}
    else:
        return ({(1, 1, ANY, ANY, 0)} |
                {(0, 1, j, ANY, cd(j, i)) for j in range(nTeams) if j != i} |
                {(1, 0, ANY, j, cd(i, j)) for j in range(nTeams) if j != i} |
                {(0, 0, j, k, cd(j, k)) for j in range(nTeams) for k in range(nTeams) if different_values(i, j, k)})


def automaton():
    qi, q01, q02, q03, q11, q12, q13 = states = "q", "q01", "q02", "q03", "q11", "q12", "q13"
    tr2 = [(qi, 0, q01), (qi, 1, q11), (q01, 0, q02), (q01, 1, q11), (q11, 0, q01), (q11, 1, q12), (q02, 1, q11), (q12, 0, q01)]
    tr3 = [(q02, 0, q03), (q12, 1, q13), (q03, 1, q11), (q13, 0, q01)]
    return Automaton(start=qi, final={q for q in states if q != qi}, transitions=tr2 if variant("a2") else tr2 + tr3)


# o[i][k] is the opponent (team) of the ith team  at the kth round
o = VarArray(size=[nTeams, nRounds], dom=range(nTeams))

# h[i][k] is 1 iff the ith team plays at home at the kth round
h = VarArray(size=[nTeams, nRounds], dom={0, 1})

# t[i][k] is the travelled distance by the ith team at the kth round. An additional round is considered for returning at home.
t = VarArray(size=[nTeams, nRounds + 1], dom=range(nTeams // 2 + 1))

satisfy(
    # a team cannot play against itself
    [o[i][k] != i for i in range(nTeams) for k in range(nRounds)],

    # ensuring predefined venues
    [venues[i][o[i][k]] == h[i][k] for i in range(nTeams) for k in range(nRounds)],

    # ensuring symmetry of games: if team i plays against j, then team j plays against i
    [o[:, k][o[i][k]] == i for i in range(nTeams) for k in range(nRounds)],

    # each team plays once against all other teams
    [AllDifferent(row) for row in o],

    # at most 'nConsecutiveGames' consecutive games at home, or consecutive games away
    [h[i] in automaton() for i in range(nTeams)],

    # handling travelling for the first game
    [(h[i][0], o[i][0], t[i][0]) in table(i, at_end=True) for i in range(nTeams)],

    # handling travelling for the last game
    [(h[i][-1], o[i][-1], t[i][-1]) in table(i, at_end=True) for i in range(nTeams)],

    # handling travelling for two successive games
    [(h[i][k], h[i][k + 1], o[i][k], o[i][k + 1], t[i][k + 1]) in table(i) for i in range(nTeams) for k in range(nRounds - 1)],

    # at each round, opponents are all different  tag(redundant-constraints)
    [AllDifferent(col) for col in columns(o)],

    # tag(symmetry-breaking)
    o[0][0] < o[0][-1]
)

minimize(
    # minimizing summed up travelled distance
    Sum(t)
)
