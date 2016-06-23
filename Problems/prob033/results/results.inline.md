 A solution of size <IT>108</IT> is reported in the
  literature
  (Anthony G. Frutos,
   Qinghua Liu,
   Andrew J. Thiel,
   Anne Marie W. Sanner,
   Anne E. Condon,
   Lloyd M. Smith, and
   Robert M. Corn,
   &#8220; Demonstration of a Word Design Strategy for DNA Computing on Surfaces,&#8221;
   Nucleic Acids Research, <B>25</B> 4748-4757 (1997).
 The solution was obtained by construction.


 <A HREF="http://maths.ucc.ie/staff/fitzpatrickp/">Coding theorists</A> at UCC
  have since then computed a solution of size <IT>112</IT>.
 This solution was obtained using a greedy algorithm.
 I have understood that the authors of the original paper
  have also found such a solution.
 (Private communication).


 <A HREF="http://csweb.ucc.ie/~dongen">I</A>
  have managed to find the solution of size <IT>112</IT>
  listed at the end of this page
  with an implementation of an optimisation CSP in
   <A HREF="http://eclipseclp.org">ECL<SUP><IT>i</IT></SUP>PS<SUP><IT>e</IT></SUP></A>.
 The solution was not easy to find in two ways.
 The first reason is that the CSP formulation is too large
  and that it is impossible to find a solution within reasonable time
  (at least with <A HREF="http://eclipseclp.org">ECL<SUP><IT>i</IT></SUP>PS<SUP><IT>e</IT></SUP></A>) and
  without a linear programming tool.
 To overcome this problem I combined the greedy and optimisation approach.
 The second reason is that the problem (at least with my formulation)
  is very sensitive to heuristics.
 This sensitivity to heuristics (choice of bases, really)
  also manifests itself with the greedy algorithms.

Here then is the solution of [size 112](length_112.md.html)

