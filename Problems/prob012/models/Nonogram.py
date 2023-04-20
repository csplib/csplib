"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python Nonogram.py -data=Nonogram_example.json
  python Nonogram.py -data=Nonogram_example.json -variant=table
  python Nonogram.py -data=Nonogram_example.txt -dataparser=Nonogram_Parser.py
"""

from pycsp3 import *

rows, cols = data  # patterns for row and columns
nRows, nCols = len(rows), len(cols)

# x[i][j] is 1 iff the cell at row i and col j is colored in black
x = VarArray(size=[nRows, nCols], dom={0, 1})

if not variant():
    def automaton(pattern):
        q = Automaton.q  # for building state names
        transitions = []
        if len(pattern) == 0:
            n_states = 1
            transitions.append((q(0), 0, q(0)))
        else:
            n_states = sum(pattern) + len(pattern)
            num = 0
            for i, size in enumerate(pattern):
                transitions.append((q(num), 0, q(num)))
                transitions.extend((q(num + j), 1, q(num + j + 1)) for j in range(size))
                transitions.append((q(num + size), 0, q(num + size + (1 if i < len(pattern) - 1 else 0))))
                num += size + 1
        return Automaton(start=q(0), final=q(n_states - 1), transitions=transitions)


    satisfy(
        [x[i] in automaton(rows[i]) for i in range(nRows)],

        [x[:, j] in automaton(cols[j]) for j in range(nCols)]
    )

elif variant("table"):
    cache = dict()


    def table(pattern, row):
        def build_from(lst, tmp, i, k):
            s = sum([pattern[e] for e in range(k, len(pattern))])
            if i + s + (len(pattern) - 1 - k) > len(tmp):
                return lst
            if i == len(tmp):
                lst.append(tuple(tmp))
            else:
                tmp[i] = 0
                build_from(lst, tmp, i + 1, k)
                if k < len(pattern):
                    for j in range(i, i + pattern[k]):
                        tmp[j] = 1
                    if i + pattern[k] == len(tmp):
                        build_from(lst, tmp, i + pattern[k], k + 1)
                    else:
                        tmp[i + pattern[k]] = 0
                        build_from(lst, tmp, i + pattern[k] + 1, k + 1)
            return lst

        key = str("R" if row else "C") + "".join(str(pattern))
        if key not in cache:
            cache[key] = build_from([], [0] * (nCols if row else nRows), 0, 0)
        return cache[key]


    satisfy(
        [x[i] in table(rows[i], row=True) for i in range(nRows)],

        [x[:, j] in table(cols[j], row=False) for j in range(nCols)]
    )
