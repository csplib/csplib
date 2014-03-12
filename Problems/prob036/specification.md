Title:    Fixed Length Error Correcting Codes
Proposer: Alan Frisch
          Chris Jefferson
          Ian Miguel
Category: Design and configuration
          Combinatorial mathematics

A fixed length error correcting code C of length n over an alphabet F
is a set of strings from F<sup>n</sup>. Given two strings from F<sup>n</sup>
we can define the distance between them. The most commonly used
distance is the Hamming distance, defined as the number of positions
where the strings differ. Using this we define the minimum distance of
C as the minimum of the distances between distinct pairs of strings
from C.

There are a number of other distances which can be used, for
example the 'Lee distance'. When F={0,1,...,n-1} the Lee distance
between a,b in F is defined as min(|a-b|,n-|a-b|). For two vectors x
and y defined over F<sup>n</sup>, the Lee distance is calculated by
summing the lee distance of the pairs x<sub>i</sub>, y<sub>i</sub>.
Most commonly F={0,1,2,3} as this case proves useful in a number of
areas.