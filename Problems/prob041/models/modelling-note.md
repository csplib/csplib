<H3> Modelling </H3>

An easy way to avoid rounding errors, is to multiply up the fractions. In
the case of the 3-fractions puzzle, this gives:
<PRE>
A(EF)(HI)+D(BC)(HI)+G(BC)(EF) == (BC)(EF)(HI)
</PRE>
Given the repetition of the original denominators, it is now useful to
bind each of these to an auxiliary variable for greater propagation.

<P>
When <EM>n</EM> is less than or equal to 3, an all-different constraint
can be used on the digit variables. Otherwise, an occurrence constraint
is used to ensure that each digit appears the correct number of times.

<!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%>
<H4> Symmetry-breaking and Implied Constraints</H4>

Symmetry in the <EM>n</EM>-fractions puzzle stems from the
commutativity of the sum operator. An obvious way to break this
symmetry is to order the fractions. For example, in the 3-fractions
puzzle:
<PRE>
A     D     G
-- <= -- <= --
BC    EF    HI
</PRE>
It is also possible to view the fractions arranged in a matrix model.
Again using the 3-fractions puzzle as an example:
<PRE>
/A D G\
|B E H|
\C F I/
</PRE>

This matrix has column symmetry only, hence all symmetry can be broken
by lexicographically ordering the columns [1]. Due to the structure of
the problem, different results are obtained by changing the order in
which the column vectors are built. For example, instead of reading
the elements top to bottom, producing &ltA, B, C&gt, &ltD, E, F&gt and
&ltG, H, I&gt, the elements can be read &ltC, B, A&gt, &ltF, E, D&gt
and &ltI, H, G&gt. Depending on the choice of symmetry-breaking
constraints, a variety of implied constraints follow, as discussed in
[2] below.

<OL>
<LI>
P. Flener, A.M. Frisch, B. Hnich, Z. Kiziltan, I. Miguel, J. Pearson, T. Walsh.
<A HREF= "http://www.cs.york.ac.uk/aig/projects/implied/docs/CPSymMatrix02.ps.gz">
Breaking Row and Column Symmetries in Matrix Models</A>.
<EM>Proceedings of the 8th International Conference on Principles and Practice
of Constraint Programming</EM>, 462--476, 2002.

<LI>
A.M. Frisch, C. Jefferson, I. Miguel.
<A HREF = "http://www.cs.york.ac.uk/aig/projects/implied/docs/SymICTR.pdf">
<EM>Symmetry-breaking as a Prelude to Implied Constraints: A Constraint Modelling
Pattern</EM></A>.
APES Technical Report 75-2003.
</OL>

