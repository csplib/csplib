Information about the Excluded Diagonals Generator and Format
=============================================================

Ian Gent, 8 Sep 2017

Relevant to our paper "Complexity of n-Queens Completion", JAIR 2017
[http://jair.org/papers/paper5512.html]()

Supporting material for our paper can be found at the following links:

+ [https://ipg.host.cs.st-andrews.ac.uk/n-queens-completion-experiments-code.tgz]()
+ [https://ipg.host.cs.st-andrews.ac.uk/n-queens-completion-experiments-results.tgz]()
+ [https://ipg.host.cs.st-andrews.ac.uk/n-queens-completion-experiments-instances.tgz]()


We have provided three Python3 files
 
+ queens-diag-gen.py 
+ queens-diag-transform.py
+ diagtoqueens.py

Excluded Diagonals Generator 
==

queens-diag-gen.py generates instances of the Excluded Diagonals problem. There 
are three arguments:
    n:          size of board and number of queens to place on it
    mininfile:  minimum number of unblocked squares required in each row/column
    seed:       an integer for Python to use as a seed



For example:

./queens-diag-gen.py 10 4 1

produces the following output on my machine


```
n =  10
numdiags =  14
diags =  [[4, 1], [2, 1], [14, 0], [8, 1], [8, 0], [7, 0], [3, 1], 
          [12, 1], [18, 0], [5, 0], [14, 1], [0, 0], [15, 1], [17, 1]]
```



The value numdiags is the number of diagonals in the list diags.  For any
number d up to numdiags a valid excluded diagonals instance can be created by
taking the FIRST d elements of the list diagas.  Other subsets should not be
taken as they may not be statistically valid.  Therefore the above example
creates 15 valid instances from d=0 up to d=14.

The list diags consists of pairs, where the first number represents a pair and
the second number the type of diagonal. 1 represents a sum diagonal and 0
represents a difference diagonal. The boards are assumed to be indexed from 0,
so the corners of a 10x10 board are (0,0) (0,9), (9,0) and (9,9).  

For sum diagonals the excluded diagonal is given by the first number in the
pair.

For difference diagonals the excluded diagonal is offset by n-1, i.e. the
difference diagonal to be excluded is the first number minus n-1. 

For example in the above, the pair [4,1] excludes sum diagonal 4, i.e.
squares (0,4), (1,3), (2,2), (3,1), (4,). While the pair [5,0] excludes
difference diagonal 5-9=-4 (since 10-1=9). That is, it excludes the squares
(0,4), (1,5), (2,6), (3,7), (4,8), (5,9).

Translation to n Queens Completion
==

Because the Excluded Diagonals problem is different to n Queens Completion, instances cannot be used directly with an n Queens Completion solver. For that reason we have created a Python translator from Excluded Diagonals to n Queens Completion. It does greatly increase the size of board but almost all queens are placed at the start.  Solutions are isomorphic in a simple way and the translated problem has the same number of solutions.

For example if the above output of queens-diag-gen.py is in file diag-10-4-1.param then the following creates an equivalent example of n Queens Completion. In this case the parameter "10" indicates that the first 10 diagonals are to be used.

```
./queens-diag-transform.py 10 < diag-10-4-1.param > completion-10-4-1.param
```

The output file starts like this with most of it omitted: 

```
letting n =  2550
letting init =  [[52, 19], [58, 31], [59, 33], [48, 11], [61, 37], [66, 47], [14, 18], 
		          [16, 14], [10, 26], [15, 16], [6, 34], [4, 38], [3, 40], [2527, 1070], 
				    ...
```

Although the value of n is massively larger, in this example n=2550, only 10 queens remain to be placed, so in fact search in the two problems is equivalent. 
Note the format is that the board size is 2550 and the bottom square is still (0,0). In this example there are queens placed at squares (52,19), (58,31) and so on.

Instances Used In The Paper
==

The following link contains all instances we used in the paper.  If you only wish the instances for Excluded Diagonals, with additional instances up to n=30, they are at this link

+ [https://ipg.host.cs.st-andrews.ac.uk/ExcludedDiagonalsInstances.zip]()

If you wish to experiment with n Queens Completion and cannot run the python translator, we have prepared a sample of 100 instances at each n from 10 to 30, with n diagonals excluded in each case.

+ [https://ipg.host.cs.st-andrews.ac.uk/ExcludedDiagonalsAsQueensCompletion.zip]()

Comparisons With Our Results
==

As we discussed in the paper, we could only solve up to n=21 using all our solvers.  The bit manipulation solver was the fastest and could also solve up to n=22. However it is unlikely to scale so currently we have no method to reliably solve instances up to n=30.
 
The results file linked above contains results on every experiment we ran. In this directory results directory you can see a file called columns.txt which has the summary.  For the excluded diagonals problems (for our results using the SAT solver Lingeling) they are
 
```
# 1 N (size of board)
# 2 M (number of blocks to place/diagonals to exclude)
# 3 instance id 
# 4 result status text
# 5 sat boolean
# 6 timed out boolean
# 7 conflicts
# 8 conflicts per sec
# 9 nodes
# 10 nodes per sec
# 11 propagations
# 12 Megapropagations per sec
# 13 solve time
# 14 total time
# 15 total system time
# 16 total wall time
# 17 filename for result data
```
 
You should be able to find the result by looking for the instances you are comparing against.  E.g. the one mentioned generated with parameters 10 4 1, and taking the first 9 queens had results
 
```
10 9 1 UNSATISFIABLE   0   0  28 4000.6  34 4857.8  717 0.1  0.0  0.21 0.03 0.20  runs-diag-2/diag-10/diag-10-9/diag-10-4-1.param-9.result
```
 
You can see this one had no solution and had a total time (column 14) of 0.21s.
You can search the data, for example to find the hardest one for this particular algorithm was n=21 excluded=19 id=259 which took about 2600s and was unsatisfiable.
