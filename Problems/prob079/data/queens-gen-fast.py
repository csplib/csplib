#!/usr/bin/env python3

import sys
import random

n = int(sys.argv[1])
m = int(sys.argv[2])
random.seed(int(sys.argv[3]))

d1 = [ 0 for i in range(2*n-1) ]
d2 = [ 0 for i in range(2*n-1) ]

validrows = [ i for i in range(n) ] 
validcols = [ j for j in range(n) ] 

def noattack(r,c):
    return ((d1[r+c] == 0) and 
            (d2[r-c+n-1] == 0) )

answer = []
queensleft = n

for attempt in range(n*n):
    i=random.randrange(queensleft)
    j=random.randrange(queensleft)
    r=validrows[i]
    c=validcols[j]
    if(noattack(r,c)): 
        answer.append([r,c]) 
        d1[r+c] = 1 
        d2[r-c+n-1] = 1
        validrows[i]=validrows[queensleft-1]
        validcols[j]=validcols[queensleft-1]
        queensleft -= 1
        if len(answer) == m:
            print("letting n = ", n)
            print("letting init = ", answer)
            sys.exit(0)

print("given up trying to generate problem")
sys.exit(1)
