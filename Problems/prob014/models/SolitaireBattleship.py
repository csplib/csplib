"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python SolitaireBattleship.py -data=SolitaireBattleship_sb-12-12-5-0.json
"""

from pycsp3 import *

fleet, hints, rowSums, colSums = data
surfaces = [ship.size * ship.cnt for ship in fleet]
pos, neg = [ship.size for ship in fleet], [-ship.size for ship in fleet]
n, nTypes = len(colSums), len(pos)


def automaton(horizontal):
    q = Automaton.q  # for building state names
    t = [(q(0), 0, q(0)), (q(0), neg if horizontal else pos, "qq"), ("qq", 0, q(0))]
    for i in pos:
        v = i if horizontal else -i
        t.append((q(0), v, q(i, 1)))
        t.extend((q(i, j), v, q(i, j + 1)) for j in range(1, i))
        t.append((q(i, i), 0, q(0)))
    return Automaton(start=q(0), final=q(0), transitions=t)


horizontal_automaton, vertical_automaton = automaton(True), automaton(False)  # automata for ships online

# s[i][j] is 1 iff the cell at row i and col j is occupied by a ship segment
s = VarArray(size=[n + 2, n + 2], dom={0, 1})

# t[i][j] is 0 iff the cell at row i and col j is unoccupied, the type (size) of the ship fragment otherwise,
# when positive, the ship is put horizontally, when negative, the ship is put vertically
t = VarArray(size=[n + 2, n + 2], dom=set(neg) | {0} | set(pos))

# cp[i] is the number of positive ship segments of type i
cp = VarArray(size=nTypes, dom=range(max(surfaces) + 1))

# cn[i] is the number of negative ship segments of type i
cn = VarArray(size=nTypes, dom=lambda i: {0} if fleet[i].size == 1 else range(max(surfaces) + 1))


def hint_ctr(c, i, j):
    if c == 'w':
        return s[i][j] == 0
    if c in {'c', 'l', 'r', 't', 'b'}:
        return [
            s[i][j] == 1,
            s[i - 1][j] == (1 if c == 'b' else 0),
            s[i + 1][j] == (1 if c == 't' else 0),
            s[i][j - 1] == (1 if c == 'r' else 0),
            s[i][j + 1] == (1 if c == 'l' else 0)
        ]
    if c == 'm':
        return [
            s[i][j] == 1,
            t[i][j] not in {-2, -1, 0, 1, 2},
            (s[i - 1][j], s[i + 1][j], s[i][j - 1], s[i][j + 1]) in {(0, 0, 1, 1), (1, 1, 0, 0)}
        ]


satisfy(
    # no ship on borders
    [(s[0][k] == 0, s[-1][k] == 0, s[k][0] == 0, s[k][-1] == 0) for k in range(n + 2)],

    # respecting the specified row tallies
    [Sum(s[i + 1]) == k for i, k in enumerate(rowSums)],

    # respecting the specified column tallies
    [Sum(s[:, j + 1]) == k for j, k in enumerate(colSums)],

    # being careful about cells on diagonals
    [(s[i][j], s[i - 1][j - 1], s[i - 1][j + 1], s[i + 1][j - 1], s[i + 1][j + 1]) in {(0, ANY, ANY, ANY, ANY), (1, 0, 0, 0, 0)}
     for i in range(1, n + 1) for j in range(1, n + 1)],

    # tag(channeling)
    [iff(s[i][j] == 1, t[i][j] != 0) for i in range(n + 2) for j in range(n + 2)],

    # counting the number of occurrences of ship segments of each type
    Cardinality(t[1:n + 1, 1:n + 1], occurrences={pos[i]: cp[i] for i in range(nTypes)} + {neg[i]: cn[i] for i in range(nTypes)}),

    # ensuring the right number of occurrences of ship segments of each type
    [cp[i] + cn[i] == surfaces[i] for i in range(nTypes)],

    # ensuring row connectedness of ship segments
    [t[i + 1] in horizontal_automaton for i in range(n)],

    # ensuring column connectedness of ship segments
    [t[:, j + 1] in vertical_automaton for j in range(n)],

    # tag(clues)
    [hint_ctr(c, i, j) for (c, i, j) in hints] if hints else None
)

