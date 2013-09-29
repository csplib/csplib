Title:    Car Sequencing
Id:       0001  
Proposer: Barbara Smith (bms@scs.leeds.ac.uk)
          test1 test2(test@example.com)
Category: Scheduling and related problems
          Some Other Category


A number of cars are to be produced; they are not identical, because different options are available as variants on the basic model. The assembly line has different stations which install the various options (air-conditioning, sun-roof, etc.). These stations have been designed to handle at most a certain percentage of the cars passing along the assembly line. Furthermore, the cars requiring a certain option must not be bunched together, otherwise the station will not be able to cope. Consequently, the cars must be arranged in a sequence so that the capacity of each station is never exceeded. For instance, if a particular station can only cope with at most half of the cars passing along the line, the sequence must be built so that at most 1 car in any 2 requires that option. The problem has been shown to be NP-complete (Gent 1999).

The format of the data files is as follows:

* First line: number of cars; number of options; number of classes.
* Second line: for each option, the maximum number of cars with that option in a block.
* Third line: for each option, the block size to which the maximum number refers.
* Then for each class: index no.; no. of cars in this class; for each option, whether or not this class requires it (1 or 0).

 
These problems have many practical applications in communications and electrical engineering. The objective is to construct a binary sequence $S_i$ of length n that minimizes the autocorrelations between bits. Each bit in the sequence takes the value +1 or -1. With non-periodic (or open) boundary conditions, the k-th autocorrelation,  $C_k$ is defined to be $\sum\limits_{i=0}^{n-k-1} S_i * S_i+k$. With periodic (or cyclic) boundary conditions, the k-th autocorrelation, $C_k$ is defined to be $\sum\limits_{i=0}^{n-1} s_i * s_{i+k\ mod\ n } $. The aim is to minimize the sum of the squares of these autocorrelations. That is, to minimize $E=\sum\limits_{k=1}^{n-1} C_k^2$.
