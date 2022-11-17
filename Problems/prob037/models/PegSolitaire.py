"""
PyCSP3 Model (see pycsp.org)

Examples:
  python PegSolitaire.py -data=[3,3,0] -variant=english
  python PegSolitaire.py -data=[3,3,0] -variant=english-hybrid 
  python PegSolitaire.py -data=[3,3,0] -variant=english-hybrid -keephybrid  
"""

from pycsp3 import *

from PegSolitaire_Generator import generate_boards

assert variant() in {"english", "3x3", "4x4", "french", "test1", "test2"} and subvariant() is None or subvariant() == "hybrid"

origin_x, origin_y, nMoves = data

init_board, final_board = generate_boards(variant(), origin_x, origin_y)

n, m = len(init_board), len(init_board[0])

horizon = sum(sum(v for v in row if v) for row in init_board) - sum(sum(v for v in row if v) for row in final_board)
nMoves = horizon if nMoves <= 0 or horizon < nMoves else nMoves
assert 0 < nMoves <= horizon

pairs = [(i, j) for i in range(n) for j in range(m) if init_board[i][j] is not None]


def build_transitions(board):
    t = []
    for i, j in pairs:
        if i + 2 < n and board[i + 2][j] is not None:
            t.append((i, j, i + 1, j, i + 2, j))
        if j + 2 < m and board[i][j + 2] is not None:
            t.append((i, j, i, j + 1, i, j + 2))
        if i - 2 >= 0 and board[i - 2][j] is not None:
            t.append((i, j, i - 1, j, i - 2, j))
        if j - 2 >= 0 and board[i][j - 2] is not None:
            t.append((i, j, i, j - 1, i, j - 2))
    return sorted(t)


transitions = build_transitions(init_board)
nTransitions = len(transitions)

# x[i,j,t] is the value at row i and column j at time t
x = VarArray(size=[nMoves + 1, n, m], dom=lambda t, i, j: {0, 1} if init_board[i][j] is not None else None)

# y[t] is the move (transition) performed at time t
y = VarArray(size=nMoves, dom=range(nTransitions))

satisfy(
    # setting the initial board
    x[0] == init_board,

    # setting the final board
    x[-1] == final_board
)

if not subvariant():
    def unchanged(i, j, t):
        valid = [k for k, tr in enumerate(transitions) if (i, j) in (tr[0:2], tr[2:4], tr[4:6])]
        return None if len(valid) == 0 else iff(conjunction(y[t] != k for k in valid), x[t, i, j] == x[t + 1, i, j])


    def to0(i, j, t):
        valid = [k for k, tr in enumerate(transitions) if (i, j) in (tr[0:2], tr[2:4])]
        return None if len(valid) == 0 else iff(disjunction(y[t] == k for k in valid), (x[t, i, j] == 1) & (x[t + 1, i, j] == 0))


    def to1(i, j, t):
        valid = [k for k, tr in enumerate(transitions) if (i, j) == tr[4:6]]
        return None if len(valid) == 0 else iff(disjunction(y[t] == k for k in valid), (x[t, i, j] == 0) & (x[t + 1, i, j] == 1))


    satisfy(
        [unchanged(i, j, t) for (i, j) in pairs for t in range(nMoves)],
        [to0(i, j, t) for (i, j) in pairs for t in range(nMoves)],
        [to1(i, j, t) for (i, j) in pairs for t in range(nMoves)]
    )

elif subvariant("hybrid"):
    def table():
        tbl = []
        for k, tr in enumerate(transitions):
            # firstly, x[t,i,j]
            t = [1 if tr[0:2] == (i, j) or tr[2:4] == (i, j) else 0 if tr[4:6] == (i, j) else ANY for i, j in pairs]
            # secondly, x[t+1,i,j]
            t += [0 if tr[0:2] == (i, j) or tr[2:4] == (i, j) else 1 if tr[4:6] == (i, j) else eq(col(k)) for k, (i, j) in enumerate(pairs)]
            # lastly, the transition (move)
            t.append(k)
            tbl.append(tuple(t))
        return tbl


    T = table()

    satisfy(
        (x[t], x[t + 1], y[t]) in T for t in range(nMoves)
    )


""" Comments
1) x[0] == init_board is possible because cells with None (either in variables or values) are discarded
   This is equivalent to [x[0][i][j] == init_board[i][j] for (i, j) in pairs]
"""
