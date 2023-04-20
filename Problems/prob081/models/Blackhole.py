"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Example:
  python Blackhole.py -data=Blackhole-01.json
"""

from pycsp3 import *

m, piles = data  # m denotes the number of cards per suit
nCards = 4 * m

# x[i] is the value j of the card at the ith position of the built stack
x = VarArray(size=nCards, dom=range(nCards))

# y[j] is the position i of the card whose value is j
y = VarArray(size=nCards, dom=range(nCards))

table = {(i, j) for i in range(nCards) for j in range(nCards) if i % m == (j + 1) % m or j % m == (i + 1) % m}

satisfy(
    # linking variables of x and y
    Channel(x, y),

    # the Ace of Spades is initially put on the stack
    y[0] == 0,

    # cards must be played in the order of the piles
    [Increasing([y[j] for j in pile], strict=True) for pile in piles],

    # each new card put on the stack must be at a rank higher or lower than the previous one
    Slide((x[i], x[i + 1]) in table for i in range(nCards - 1))
)

""" Comments
1) Slide is only used to have more compact XCSP3 instances
   we could have written: [(x[i], x[i + 1]) in table for i in range(nCards - 1)]
"""
