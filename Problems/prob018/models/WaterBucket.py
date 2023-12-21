"""
PyCSP3 Model (see pycsp.org)

Examples:
  python WaterBucket.py
  python WaterBucket.py -data=[8,5,3,4,4,0,8]
"""

from pycsp3 import *

# ci for capacities of the three buckets
# gi for goal (which quantities must be present in the three buckets after all transfers)
# h for horizon (maximal number of rounds/transfers)
c1, c2, c3, g1, g2, g3, h = data or (8, 5, 3, 4, 4, 0, 8)

assert c1 >= c2 >= c3 > 0, "Bucket capacities must be in decreasing order"
assert g1 + g2 + g3 == c1, "water from bucket 1 must be split into the three buckets to reach the goal"


def tables():
    def nearby_states(a1, a2, a3, b1, b2, b3):
        if (a1, a2, a3) == (g1, g2, g3):
            return (b1, b2, b3) == (g1, g2, g3)  # when the goal is reached, buckets are kept as they are
        if a1 != b1 and a2 != b2 and a3 != b3:  # not a possible transition if three buckets are impacted
            return False
        if a1 != b1 and a2 != b2:
            return b1 in {0, c1} or b2 in {0, c2}
        if a1 != b1 and a3 != b3:
            return b1 in {0, c1} or b3 in {0, c3}
        if a2 != b2 and a3 != b3:
            return b2 in {0, c2} or b3 in {0, c3}
        return False  # not related in other cases

    states = {(i, j, k) for i in range(c1 + 1) for j in range(c2 + 1) for k in range(c3 + 1) if i + j + k == c1}
    table_tr = {(*state1, *state2) for state1 in states for state2 in states if nearby_states(*state1, *state2)}
    table_hr = {(t,) + (ANY,) * (t * 3) + (g1, g2, g3) * (h - t) for t in range(h)}
    return table_tr, table_hr


table_transitions, table_horizon = tables()

# x[t][i] is the volume of water in bucket i at time (round) t
x = VarArray(size=[h, 3], dom=lambda t, i: range(c1 + 1) if i == 0 else range(c2 + 1) if i == 1 else range(c3 + 1))

# z is the number of transfers of water in order to reach the goal
z = Var(dom=range(h))

satisfy(
    # Initially, at round 0, the bucket 1 is full of water while the other buckets are empty
    x[0] == (c1, 0, 0),

    # only some transfers are possible between two successive rounds
    [(x[t] + x[t + 1]) in table_transitions for t in range(h - 1)],  # or (*x[t], *x[t + 1])

    # computing the number of transfers
    (z, *flatten(x)) in table_horizon
)

minimize(
    # minimizing the number of transfers
    z
)

