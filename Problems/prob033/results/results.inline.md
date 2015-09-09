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

<P>

 <A HREF="http://maths.ucc.ie/staff/fitzpatrickp/">Coding theorists</A> at UCC 
  have since then computed a solution of size <IT>112</IT>.
 This solution was obtained using a greedy algorithm. 
 I have understood that the authors of the original paper
  have also found such a solution.
 (Private communication).

<P>

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

Here then is the solution of size 112: 
<P>

 <CENTER>
 <TABLE COLUMNS=8>
<TR><TD>AAGCCGTT</TD> <TD>TACGCGAT</TD> <TD>TAGCGCAT</TD> <TD>TTCGGCAA</TD></TR>
<TR><TD>ATGGGGAT</TD> <TD>TAGGGGTA</TD> <TD>TTCCGGTT</TD> <TD>TTGGCCTT</TD></TR>
<TR><TD>AGACCACT</TD> <TD>ACTCCTGT</TD> <TD>AGAGCTGA</TD> <TD>AGTCGAGA</TD></TR>
<TR><TD>TGAGGACA</TD> <TD>TCTGGTGA</TD> <TD>TGACGTGT</TD> <TD>TGTGCAGT</TD></TR>
<TR><TD>ACCTTGGA</TD> <TD>AGCATCGT</TD> <TD>TGCTACGA</TD> <TD>TGGAACCT</TD></TR>
<TR><TD>AGGTAGGT</TD> <TD>TCGTTCGT</TD> <TD>TGCTTGCT</TD> <TD>TGGATGGA</TD></TR>
<TR><TD>CAACTCCT</TD> <TD>CAAGACGA</TD> <TD>CTACAGCA</TD> <TD>GATCACCA</TD></TR>
<TR><TD>CATCAGGT</TD> <TD>CATGTGCA</TD> <TD>CTTCTCGA</TD> <TD>CTTGACCT</TD></TR>
<TR><TD>GAACTGGA</TD> <TD>GAAGAGCT</TD> <TD>GTACACGT</TD> <TD>GTAGTCCA</TD></TR>
<TR><TD>CTAGTGGT</TD> <TD>GATGTCGT</TD> <TD>GTTCTGCT</TD> <TD>GTTGAGGA</TD></TR>
<TR><TD>CTTCCTAG</TD> <TD>GAAGCTAG</TD> <TD>GAAGGATC</TD> <TD>GATCGAAG</TD></TR>
<TR><TD>CATGGTTG</TD> <TD>GTACGTTG</TD> <TD>GTTGCATG</TD> <TD>GTTGGTAC</TD></TR>
<TR><TD>CAGAAGTG</TD> <TD>CTCATGTC</TD> <TD>GAGTACAG</TD> <TD>GTCTTCAC</TD></TR>
<TR><TD>CTGTTGAG</TD> <TD>GAGTTGTC</TD> <TD>GTCTAGTG</TD> <TD>GTGATCTG</TD></TR>
<TR><TD>CACACACT</TD> <TD>CACTGTCA</TD> <TD>CAGAGAGA</TD> <TD>CTCACTGA</TD></TR>
<TR><TD>CTGTCACA</TD> <TD>GACTCAGA</TD> <TD>GAGACTCA</TD> <TD>GTCAGACA</TD></TR>
<TR><TD>CAGTCTGT</TD> <TD>CTCTGAGT</TD> <TD>CTGAGTCT</TD> <TD>GACAGTGT</TD></TR>
<TR><TD>GAGTGACT</TD> <TD>GTCTCTCT</TD> <TD>GTGACAGT</TD> <TD>GTGTGTGA</TD></TR>
<TR><TD>CGAAAACG</TD> <TD>CCATTAGG</TD> <TD>CGAATTGC</TD> <TD>GCTTAACG</TD></TR>
<TR><TD>GGTAATCC</TD> <TD>GCTTTTGC</TD> <TD>GGATATGG</TD> <TD>GGTATAGG</TD></TR>
<TR><TD>CCAACCTT</TD> <TD>CCAAGGAA</TD> <TD>CCTTCCAA</TD> <TD>GGAACCAA</TD></TR>
<TR><TD>CGATCGAT</TD> <TD>CGATGCTA</TD> <TD>CGTACGTA</TD> <TD>CGTAGCAT</TD></TR>
<TR><TD>GCATCGTA</TD> <TD>GCATGCAT</TD> <TD>GCTACGAT</TD> <TD>GCTAGCTA</TD></TR>
<TR><TD>CCTTGGTT</TD> <TD>GGAAGGTT</TD> <TD>GGTTCCTT</TD> <TD>GGTTGGAA</TD></TR>
<TR><TD>CCCCAAAA</TD> <TD>CCCGTATT</TD> <TD>CCGCTTTA</TD> <TD>CCGGATAT</TD></TR>
<TR><TD>CGCCTTAT</TD> <TD>CGCGATTA</TD> <TD>CGGCAATT</TD> <TD>CGGGTAAA</TD></TR>
<TR><TD>GCCCATTT</TD> <TD>GCCGTTAA</TD> <TD>GCGCTAAT</TD> <TD>GCGGAATA</TD></TR>
<TR><TD>GGCCTATA</TD> <TD>GGCGAAAT</TD> <TD>GGGCATAA</TD> <TD>GGGGTTTT</TD></TR>
 </TABLE>
</CENTER>
