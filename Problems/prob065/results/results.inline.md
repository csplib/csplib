For 22 of the following 23 instances (available in [MiniZinc format](../data/instances_mzn.zip) and [Essence format](../data/instances_essence.zip)), the computed lower bound (lb) on $\lambda$ has been shown to be feasible. 

$v$	|  $b$	|  $r$	| lb($\lambda$)
-------:|------:|------:|-------------:
 9	| 300	| 100	|     25
10	| 325	| 100	|     24
10	| 350	| 100	|     22
10	| 360	| 120	|     32
15	| 350	| 100	|     24
19	|  20	|   9	|      4
 9	|  70	|  35	|     16
10	| 100	|  30	|      7
11	| 150	|  50	|     14
12	| 200	|  75	|     25
13	| 250	|  80	|     22
 6	|  50	|  25	|     10
 6	|  60	|  30	|     12
 8	|  28	|  14	|      6
 9	|  36	|  12	|      3
10	|  30	|   9	|      2
11	|  22	|  10	|      4
12	|  44	|  11	|      2
13	|  26	|   6	|      1
15	|  21	|   7	|      2$^*$ 
16	|  16	|   6	|      2
19	|  19	|   9	|      4
10	|  37	|  14	|      5

$^*$ For the instance $\langle 15,21,7 \rangle$, the best known solution has $\lambda = 3$ and it is unkown whether there exists a solution that matches the computed lower bound of 2.