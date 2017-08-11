#!/usr/bin/env python3

# arguments
# 1: size of board
# 2: number of squares to block
# 3: min to lreave remaining in each row/column
# 4: random seed

# note that you must have (1)*(1-3) >= (2)


import sys
import random

n = int(sys.argv[1])
m = int(sys.argv[2])
mininfile = int(sys.argv[3])
random.seed(int(sys.argv[4]))

for attempt in range(1000):
    queensorder = [ [i,j] for i in range(n) for j in range(n) ]
    random.shuffle(queensorder)

    rows = [ [j for j in range(n)]  for i in range(n) ]
    cols = [ [j for j in range(n)] for i in range(n) ]

    answer = []

    for q in queensorder:
        r = q[0]
        c = q[1]
        if len(rows[r]) > mininfile and len(cols[c]) > mininfile: 
            rows[r].remove(c)
            cols[c].remove(r)
            answer.append([r+1,c+1]) # because old blocked queens instances are from 1 
            if len(answer) == m:
                answer.sort()
                print("letting n = ", n)
                print("letting blocks = ", answer)
                sys.exit(0)



