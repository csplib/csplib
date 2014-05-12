###Problem instances

The 2 differentiation problem instances shown here come from a more complete set of [benchmarks](http://ssdi.di.fct.unl.pt/~fa/differential-diagnosis/benchmarks.html). One is for single faults; the other, for double faults. Both recur to the c1355 ISCAS'85 circuit. Circuit description (taken from [http://www.eecs.umich.edu/~jhayes/iscas/](http://www.eecs.umich.edu/~jhayes/iscas/) ) can be seen [here](http://ianm.host.cs.st-andrews.ac.uk/CSPLib/prob/prob043/c1355.txt).

The 2 benchmarks are labelled b1355_a_1b (single faults) and b1355_a_2 (double faults). Each consists of a set of diagnoses to differentiate. To make testing more exhaustive, and since there is no 'real' circuit with a real fault, the goal is to partition such set into sets of equivalence classes of diagnoses (since some diagnoses may be indistinguishable) by means of pair-wise differentiations.
Notice that diagnoses of double faults are not necessarily more difficult to differentiate than single faults.
The sets of diagnoses for the 2 benchmarks are shown below, together with the respective result of equivalence classes.


### Diagnoses and respective Classes


####Benchmark *b1355_a_2*

<table width="549" border="1">
  <tbody><tr>
    <td width="84"><span><font size="2">5 Diagnoses</font></span></td>
    <td width="449"><span><font size="2">['1081gat'/1], 
    ['1177gat'/0], ['1258gat'/1], ['1307gat'/0], ['1339gat'/0]</font></span></td>
  </tr>
  <tr>
    <td width="84"><span><font size="2">3 Classes</font></span></td>
    <td width="449"><span><font size="2">[[1258gat/1]], 
    [[1339gat/0],[1307gat/0],[1177gat/0]], [[1081gat/1]]</font></span></td>
  </tr>
</tbody></table>

#### Benchmark *b1355_a_2*

<table width="59%" border="1">
  <tbody><tr>
    <td nowrap="" width="15%"><span><font size="2">36 Diagnoses</font></span></td>
    <td nowrap="" width="85%"><span><font size="2">
    ['1039gat'/1,'1051gat'/1],['1039gat'/1,'1147gat'/0],['1039gat'/1,'1238gat'/1],['1039gat'/1,'1297gat'/0],['1039gat'/1,'1329gat'/0],['1051gat'/1,'1230gat'/0],['1051gat'/1,'1231gat'/0],
    <br>
    ['1051gat'/1,'1293gat'/1],['1051gat'/1,'1325gat'/1],['1147gat'/0,'1230gat'/0],['1147gat'/0,'1231gat'/0],['1147gat'/0,'1293gat'/1],['1147gat'/0,'1325gat'/1],['1230gat'/0,'1238gat'/1],
    <br>
    ['1230gat'/0,'1297gat'/0],['1230gat'/0,'1329gat'/0],['1231gat'/0,'1238gat'/1],['1231gat'/0,'1297gat'/0],['1231gat'/0,'1329gat'/0],['1238gat'/1,'1293gat'/1],['1238gat'/1,'1325gat'/1],
    <br>
    ['1293gat'/1,'1297gat'/0],['1293gat'/1,'1329gat'/0],['1297gat'/0,'1325gat'/1],['1325gat'/1,'1329gat'/0],['36gat'/0,'1039gat'/1],['36gat'/0,'1230gat'/0],['36gat'/0,'1231gat'/0],
    <br>
    ['36gat'/0,'1293gat'/1],['36gat'/0,'1325gat'/1],['8gat'/1,'1051gat'/1],['8gat'/1,'1147gat'/0],['8gat'/1,'1238gat'/1],['8gat'/1,'1297gat'/0],['8gat'/1,'1329gat'/0],['8gat'/1,'36gat'/0]</font></span></td>
  </tr>
  <tr>
    <td nowrap="" width="15%"><span><font size="2">12 Classes</font></span></td>
    <td nowrap="" width="85%"><span><font size="2">[[8gat/1,36gat/0]], 
    [[8gat/1,1238gat/1]], 
    [[8gat/1,1329gat/0],[8gat/1,1297gat/0],[8gat/1,1147gat/0]], 
    [[8gat/1,1051gat/1]], [[36gat/0,1325gat/1],[36gat/0,1293gat/1], <br>
    [36gat/0,1231gat/0],[36gat/0,1230gat/0]], [[36gat/0,1039gat/1]], 
    [[1238gat/1,1325gat/1],[1238gat/1,1293gat/1],[1231gat/0,1238gat/1],[1230gat/0,1238gat/1]],
    <br>
    [[1325gat/1,1329gat/0],[1297gat/0,1325gat/1],[1293gat/1,1329gat/0],[1293gat/1,1297gat/0],[1231gat/0,1329gat/0],[1231gat/0,1297gat/0],[1230gat/0,1329gat/0],
    <br>
    [1230gat/0,1297gat/0],[1147gat/0,1325gat/1],[1147gat/0,1293gat/1],[1147gat/0,1231gat/0],[1147gat/0,1230gat/0]], 
    [[1051gat/1,1325gat/1],[1051gat/1,1293gat/1], <br>
    [1051gat/1,1231gat/0],[1051gat/1,1230gat/0]], [[1039gat/1,1238gat/1]], 
    [[1039gat/1,1329gat/0],[1039gat/1,1297gat/0],[1039gat/1,1147gat/0]], 
    [[1039gat/1,1051gat/1]]</font></span></td>
  </tr>
</tbody></table>