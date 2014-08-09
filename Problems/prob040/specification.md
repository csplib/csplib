---
Title:    A Distribution Problem with Wagner-Whitin Costs
Proposer: 
	- Ian Miguel
	- Armagan Tarim
Category: 
	- Scheduling and related problems
    - Bin packing
---

A basic distribution problem is described as follows.
Given:
<UL>
<LI> A supply chain structure of <em>stocking points</em> divided into
<em>L</em> <em>levels</em>, such as:
<pre>
              |                   Level
              V
            +---+
            | F |                   3
            +---+
          _/     \_
         /         \
        V           V
      +---+       +---+
      | D |       | E |             2
      +---+       +---+
    _/     \_          \_
   /         \           \
  V           V           V
+---+       +---+       +---+
| A |       | B |       | C |       1
+---+       +---+       +---+
  |           |           |
  V           V           V
</pre>
<LI>A <em>holding cost</em> (<em>c</em>) per unit of inventory at each
    stocking point, where it is assumed that a parent has lower holding
    cost than any of its children.
<LI>A <em>procurement cost</em> (<em>c0</em>) per stocking point
    (per order, not per unit of inventory received).
<LI>A number of periods, <em>T</em>.
<LI>A <em>demand</em> for each leaf (<em>A - C</em>) at each period.
</UL>

Find an optimal ordering <em>policy</em>: i.e. a decision as to how much
to order at each stocking point at each time period that minimises cost.

The Wagner-Whitin form of the problem assumes that the holding costs
and procurement costs are constant, and that the demands are known for
the entire planning horizon. Furthermore, the stocking points have no
maximum capacity and the starting inventory is 0.