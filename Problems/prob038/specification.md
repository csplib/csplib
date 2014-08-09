---
Title:    Steel Mill Slab Design
Proposer: Ian Miguel
Category: 
    - Design and configuration
    - Bin packing
---

<h3>Specification - Type 1</h3>

Steel is produced by casting molten iron into slabs. A steel mill can
produce a finite number, <EM>&sigma;</EM>, of <EM>slab sizes</EM>. An
order has two properties, a <em>colour</em> corresponding to the route
required through the steel mill and a <em>weight</em>. Given
<em>d</em> input orders, the problem is to assign the orders to slabs,
the number and size of which are also to be determined, such that the
total weight of steel produced is minimised. This assignment is
subject to two further constraints:

<OL>
<LI><B>Capacity constraints</B>: The total weight of orders assigned to a slab
    cannot exceed the slab capacity.
<LI><B>Colour constraints</B>: Each slab can contain at most <EM>p</EM> of
    <EM>k</EM> total colours (<EM>p</EM> is usually 2).
</OL>

The colour constraints arise because it is expensive to cut up slabs
in order to send them to different parts of the mill.

The above description is a simplification of a real industrial problem
(see [1]). For example, the problem may also include <EM>inventory
matching</EM>, where surplus stock can be used to fulfil some of the orders.

<h3>Specification - Type 2</h3>

The Type 1 specification does not constrain the number of slabs
used. The Type 2 specification (see [2]) extends the objective to
require further that the number of slabs used to accommodate the
minimal weight of steel is also minimised.

<OL>
<LI>J. R. Kalagnanam, M. W. Dawande, M. Trumbo, H. S. Lee.
    "<A HREF="http://domino.watson.ibm.com/library/cyberdig.nsf/0/b5a5d1e00fe8db8d85256611004ae781?OpenDocument">Inventory
    Matching Problems in the Steel Industry</A>,"
    IBM Research Report RC 21171, 1998.

<LI>A. Gargani, P. Refalo.  An Efficient Model and Strategy for the
    Steel Mill Slab Design Problem.  Proceedings of the 13th
    International Conference on Principles & Practice of Constraint
    Programming, 77-89, 2007.  </OL>