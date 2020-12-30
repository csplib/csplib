#!/usr/bin/env python3

import sys
import random
import re
import diagtoqueens

if len(sys.argv) < 2: 
     print("Usage:", sys.argv[0],end="")
     print(" numdiags  ")
     print("Reads n and diagonal choices from stdin and prints out translation into n queens completion")
     sys.exit(0)

numdiags = int(sys.argv[1])

numbers = [int(s) for s in re.findall('\d+', sys.stdin.read())]

n = numbers[0]
maxdiags = numbers[1]

if maxdiags < numdiags:
    print("more diagonals requested than available in input ", maxdiags, " < ", numdiags)
    sys.exit(1)

diagtoqueens.printqueensfromdiag(numdiags,numbers)

sys.exit(0)
