"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python Rehearsal.py -data=rehearsalSmith.json
  python Rehearsal.py -data=rehearsalSmith.json -variant=bis
"""

from pycsp3 import *

durations, playing = data
nPieces, nPlayers = len(durations), len(playing)

# o[i] is the piece played in slot (order) i
o = VarArray(size=nPieces, dom=range(nPieces))

# a[p] is the (beginning of the) slot  when the player p arrives
a = VarArray(size=nPlayers, dom=range(nPieces))

# l[p] is the (end of the) slot when the player p leaves
l = VarArray(size=nPlayers, dom=range(nPieces))

if not variant():

    satisfy(
        # all pieces of music must be played in some order
        AllDifferent(o),

        # a player must be present when a piece of music requires him/her
        [(playing[p][o[i]] == 0) | (a[p] <= i) & (i <= l[p]) for p in range(nPlayers) for i in range(nPieces)]
    )

    minimize(
        # minimizing the waiting time of players (i.e. without playing)
        Sum(durations[o[i]] * ((playing[p][o[i]] == 0) & (a[p] <= i) & (i <= l[p])) for p in range(nPlayers) for i in range(nPieces))
    )

elif variant("bis"):

    # ep[p][i] is 1 iff the player p must effectively play in slot i
    ep = VarArray(size=[nPlayers, nPieces], dom={0, 1})

    satisfy(
        # all pieces of music must be played in some order
        AllDifferent(o),

        # determining when players must effectively play
        [ep[p][i] == playing[p][o[i]] for p in range(nPlayers) for i in range(nPieces)],

        # a player must be present when a piece of music requires him/her
        [(ep[p][i] == 0) | (a[p] <= i) & (i <= l[p]) for p in range(nPlayers) for i in range(nPieces)]
    )

    minimize(
        # minimizing the waiting time of players (i.e. without playing)
        Sum(durations[o[i]] * ((ep[p][i] == 0) & (a[p] <= i) & (i <= l[p])) for p in range(nPlayers) for i in range(nPieces))
    )

""" Comments
1) the first model variant is very compact. The second model variant explicitly introduces some auxiliary variables
   which, to som respect, allows a better control of the generated instances. Here, however, the outputs  are not
   so different for this problem.
"""
