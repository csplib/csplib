"""
PyCSP3 Model (see pycsp.org)

Examples:
  python StillLife.py
  python StillLife.py -data=[7,7]
  python StillLife.py -data=[7,7] -variant=wastage
"""

from pycsp3 import *

n, m = data or (8, 8)


if not variant():
    table = {(v, 0) for v in range(9) if v != 3} | {(2, 1), (3, 1)}


    def scope(i, j):
        return [x[k][l] for k in range(n) for l in range(m) if i - 1 <= k <= i + 1 and j - 1 <= l <= j + 1 and (k, l) != (i, j)]


    # x[i][j] is 1 iff the cell at row i and col j is alive
    x = VarArray(size=[n, m], dom={0, 1})

    # a[i][j] is the number of alive neighbours
    a = VarArray(size=[n, m], dom=range(9))

    satisfy(
        # computing the numbers of alive neighbours
        [Sum(scope(i, j)) == a[i][j] for i in range(n) for j in range(m)],

        # imposing rules of the game
        [(a[i][j], x[i][j]) in table for i in range(n) for j in range(m)],

        # imposing rules for ensuring valid dead cells around the board
        [
            [x[0][i:i + 3] != (1, 1, 1) for i in range(m - 2)],
            [x[-1][i: i + 3] != (1, 1, 1) for i in range(m - 2)],
            [x[i:i + 3, 0] != (1, 1, 1) for i in range(n - 2)],
            [x[i:i + 3, - 1] != (1, 1, 1) for i in range(n - 2)]
        ],

        # tag(symmetry-breaking)
        (x[0][0] >= x[n - 1][n - 1], x[0][n - 1] >= x[n - 1][0]) if n == m else None
    )

    maximize(
        # maximizing the number of alive cells
        Sum(x)
    )

elif variant("wastage"):
    assert n == m


    def condition_for_tuple(t0, t1, t2, t3, t4, t5, t6, t7, t8, wa):
        s3 = t1 + t3 + t5 + t7
        s1 = t0 + t2 + t6 + t8 + s3
        s2 = t0 * t2 + t2 * t8 + t8 * t6 + t6 * t0 + s3
        return (t4 != 1 or (2 <= s1 <= 3 and (s2 > 0 or wa >= 2) and (s2 > 1 or wa >= 1))) and \
               (t4 != 0 or (s1 != 3 and (0 < s3 < 4 or wa >= 2)) and (s3 > 1 or wa >= 1))


    table = {(*t, i) for t in product(range(2), repeat=9) for i in range(3) if condition_for_tuple(*t, i)}

    # x[i][j] is 1 iff the cell at row i and col j is alive (note that there is a border)
    x = VarArray(size=[n + 2, n + 2], dom=lambda i, j: {0} if i in {0, n + 1} or j in {0, n + 1} else {0, 1})

    # w[i][j] is the wastage for the cell at row i and col j
    w = VarArray(size=[n + 2, n + 2], dom={0, 1, 2})

    # ws[i] is the wastage sum for cells at row i
    ws = VarArray(size=n + 2, dom=range(2 * (n + 2) * (n + 2) + 1))

    satisfy(
        # ensuring that cells at the border remain dead
        [
            [x[1][j:j + 3] != (1, 1, 1) for j in range(n)],
            [x[n][j:j + 3] != (1, 1, 1) for j in range(n)],
            [x[i:i + 3, 1] != (1, 1, 1) for i in range(n)],
            [x[i:i + 3, n] != (1, 1, 1) for i in range(n)]
        ],

        # still life + wastage constraints
        [(x[i - 1:i + 2, j - 1:j + 2], w[i][j]) in table for i in range(1, n + 1) for j in range(1, n + 1)],

        # managing wastage on the border
        [
            [(w[0][j] + x[1][j] == 1, w[n + 1][j] + x[n][j] == 1) for j in range(1, n + 1)],
            [(w[i][0] + x[i][1] == 1, w[i][n + 1] + x[i][n] == 1) for i in range(1, n + 1)],
        ],

        # summing wastage
        [Sum(w[0] if i == 0 else [ws[i - 1], w[i]]) == ws[i] for i in range(n + 2)],

        # tag(redundant-constraints)
        [ws[n + 1] - ws[i] >= 2 * ((n - i) // 3) + n // 3 for i in range(n + 1)]
    )

    maximize(
        # maximizing the number of alive cells
        (2 * n * n + 4 * n - ws[-1]) // 4
    )

