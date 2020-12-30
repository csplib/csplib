In the Industrial Modelling Competition at CP 2015, only a subset of the instances were used. Results are shown in the table below, giving the best lower bound, the best upper bound and an indication if the solution is optimal. Results were obtained by Philippe Laborie with IBM's CP Optimizer. The OPL program used is shown in the model section.

The lower bound is computed from a graph of the binary disjunctive constraints between tests. The nodes of the graph are the tests, with weights given by their duration, two nodes are linked if there exists a disjunctive constraint on both tasks. The cost of the maximal weighted clique in that graph is a lower bound for the schedule, as no two tasks in the clique can be run concurrently. 

The tasks of the clique are also a good subset of activities to schedule first.


|  Instance |                     LB     |                 UB  |   OPTIMAL|
| ---|---:|---:|---|
|t40m10r3-2.pl    |      1725    |                1725 |  *  |
|t50m10r3-9.pl     |     7279     |               7279  | *|
|t100m50r10-11.pl|       4970   |                 4970|   *|
|t500m50r5-5.pl     |    33848    |               33848|  *|
|t500m100r10-1.pl  |     48814   |                48814 | *|
|t500m100r10-2.pl   |    38303    |               39130| |
|t500m100r10-3.pl    |   35459     |              36018| |
|t500m100r10-4.pl     |  37658      |             37921| |
|t500m100r10-5.pl      | 33080       |            33338| |
|t500m100r10-6.pl |      41078        |           41078 | *|
|t500m100r10-7.pl  |     38921         |          39790||
|t500m100r10-8.pl   |    42455          |         43199||
|t500m100r10-9.pl    |   38758           |        39083||
|t500m100r10-10.pl   |   42308           |        42308|  *|
