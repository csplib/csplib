---
Title:    Winner Determination Problem (Combinatorial Auction)
Proposer: Patrick Prosser
Category: Design and configuration
---

There is a bunch of people bidding for things. A bid has a value, and the bid is for a set of items. 
If we have two bids, call them A and B, and there is an intersection on the items they bid for, then we can
accept bid A or bid B, but we cannot accept both of them. However, if A and B are bids on disjoint sets 
of items then these two bids are compatible with each other, and we might accept both. The problem then is 
to accept compatible bids such that 
we maximise the sum of the values of those bids (i.e. make most money).

There's a nice wiki on this problem here https://en.wikipedia.org/wiki/Combinatorial_auction
and there is a set of problem instances here http://www.dcs.gla.ac.uk/~pat/maxWeightClique/WDPinstance/ 
