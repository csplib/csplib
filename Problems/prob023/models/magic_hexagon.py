#!/usr/bin/python
"""
Magic hexagon in Numberjack.

Prob023: Magic Hexagon
http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/
'''
A magic hexagon consists of the number 1 to 19 arranged in a hexagonal pattern:

   A,B,C
  D,E,F,G
 H,I,J,K,L
  M,N,O,P
   Q,R,S

We have a constraint that all diagonals sum to 38. That is,
   A+B+C = D+E+F+G = ... = Q+R+S = 38, A+D+H = B+E+I+M = ... = L+P+S = 38,
   C+G+L = B+F+K+P = ... = H+M+Q = 38.
'''

Compare with the following models:
* SICStus Prolog: http://www.hakank.org/sicstus/magic_hexagon.pl
* ECLiPSe: http://www.hakank.org/eclipse/magic_hexagon.ecl

This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
See also my Numberjack page http://www.hakank.org/numberjack/

"""
from Numberjack import *

class MyAllDiff(Predicate):
    
    def __init__(self, vars):
        Expression.__init__(self, "MyAllDiff")
        self.set_children(vars)

    def decompose(self):
        return [var1 != var2 for var1, var2 in pair_of(self.children)]


def model(libs):

    N = 19
    LD = VarArray(N, 1, N, 'LD')
    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s = LD

    model = Model (
        AllDiff(LD),
        # MyAllDiff(LD), # FIX!
        a + b + c ==  38,
        d + e + f + g ==  38,
        h + i + j + k + l ==  38, 
        m + n + o + p ==  38, 
        q + r + s ==  38, 
        a + d + h ==  38, 
        b + e + i + m ==  38, 
        c + f + j + n + q ==  38, 
        g + k + o + r ==  38, 
        l + p + s ==  38, 
        c + g + l ==  38, 
        b + f + k + p ==  38, 
        a + e + j + o + s ==  38, 
        d + i + n + r ==  38, 
        h + m + q ==  38, 
        a < c,
        a < h,
        a < l,
        a < q,
        a < s,
        c < h
        )

    print model

    for library in libs:
        solver = model.load(library) # Load up model into solver
        print ''
        if solver.solve():
            solver.printStatistics()
            print 'LD:', LD
            print ''
            num_solutions = 1
            while solver.getNextSolution() == SAT: 
                num_solutions += 1
                print 'LD:', LD
                print ''
            print 'Number of solutions: ', num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            print 'getPropags:', solver.getPropags()
            print 'getBacktracks:', solver.getBacktracks()
            print 'getFailures:', solver.getFailures()
        else:
            print 'No solution'
        print ''


model(['Mistral'])


