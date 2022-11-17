"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python Auction.py
  python Auction.py -data=Auction_example.json
"""

from pycsp3 import *

bids = data or default_data("Auction_example.json")
items = sorted({item for bid in bids for item in bid.items})
values = integer_scaling(bid.value for bid in bids)
nBids = len(bids)

# x[i] is 1 iff the ith bid is selected
x = VarArray(size=nBids, dom={0, 1})

satisfy(
    # avoiding intersection of bids
    Count(scp, value=1) <= 1 for scp in [[x[i] for i, bid in enumerate(bids) if item in bid.items] for item in items]
)

maximize(
    # maximizing summed values of selected bids
    x * values
)
