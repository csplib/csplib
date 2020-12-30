#!/usr/bin/env python3

import sys
import random
import re


numbers = [int(s) for s in re.findall('\d+', sys.stdin.read())]

if len(numbers) <= 1: 
     print("Usage:", sys.argv[0]) 
     print("  Read from standard input in format:", "n queen1x queen1y queen2x queen2y ...") 
     print("  All non-digit/whitespace characters ignored")
     sys.exit(0)
 
n = int(numbers[0])
outputComments = True

rows = [ 0 for i in range(n) ]
cols = [ 0 for i in range(n) ]
d1 = [ 0 for i in range(2*n-1) ]
d2 = [ 0 for i in range(2*n-1) ]

numSatVars = 0

def readqueens():
    numargs = int(len(numbers)-1)
    for i in range(int(numargs/2)):
        queenx=int(numbers[i*2+1])
        queeny=int(numbers[i*2+2])
        if noattack([queenx,queeny]):
            rows[queenx] = 1 
            cols[queeny] = 1 
            d1[queenx+queeny] = 1 
            d2[queenx-queeny+n-1] = 1
        else:
            outputFalseSatInstance()

def noattack(q):
    return (rows[q[0]] == 0 and
            cols[q[1]] == 0 and
            (d1[q[0]+q[1]] == 0) and 
            (d2[q[0]-q[1]+n-1] == 0) )


def queen2var(q):
    global numSatVars
    rawInt = q[0]*n+q[1]
    if rawInt not in int2var:
        numSatVars += 1
        qv = len(int2var)+1
        int2var[rawInt] = qv
        var2queen[qv] = q
    return int2var[rawInt]

def processqueens():
    global outrows
    global outcols
    global outd1
    global outd2
    outrows = [ [] for i in range(n) ]
    outcols = [ [] for i in range(n) ]
    outd1 = [ [] for i in range(2*n-1) ]
    outd2 = [ [] for i in range(2*n-1) ]
    openrows = []
    opencols = []
    for i in range(n):
        if rows[i] == 0:
            openrows.append(i)
        if cols[i] == 0:
            opencols.append(i)
    for i in openrows:
        for j in opencols:
            q=[i,j]
            if noattack(q):
                qv = queen2var(q)
                outrows[i].append(qv)
                outcols[j].append(qv)
                outd1[i+j].append(qv)
                outd2[i-j+n-1].append(qv)
    for i in openrows: 
        if outrows[i] == []:
            outputComplexFalseSatInstance()
    for i in opencols: 
        if outcols[i] == []:
            outputComplexFalseSatInstance()

int2var = {} 
var2queen = {} 

def outputFalseSatInstance():
    print("c Unsatisfiability detected in preprocessing")
    print("c ")
    print("p cnf 1 1")
    print("0")
    sys.exit(0)

def outputComplexFalseSatInstance():
    print("c This instance contains a row or column where no queens can be placed")
    outputFalseSatInstance()

#### Commander Encoding


clauses = []
maxSize = 3

def varAlloc():
    global numSatVars
    numSatVars += 1
    return numSatVars

def makeCommanderClauses(varList,commander):
    global clauses
    numVars = len(varList)
    # if commander true then at least one is true 
    c1 = list(map(lambda x: x,varList))
    c1.append(-commander)
    clauses.append(c1)
    # if commander is false then all false
    for var in varList:
        clauses.append([commander,-var])
    # naive at most one for vars
    for i in range(numVars-1):
        for j in range(i+1,numVars):
            clauses.append([-varList[i],-varList[j]])


# returns variable which is equivalent to the or of variables in the varList
# adds needed clauses to the global clauses list to enforce this and that at most one of varlist is true 

def makeCommanderFromVars(varList):
    if not isinstance(varList,list): # assume that input is a single variable
        return varList
    numVars = len(varList)
    if numVars == 1:
        return varList[0]           # no extra clauses necessary
    commander = varAlloc()
    makeCommanderClauses(varList,commander)
    return commander

    
def groupVarsAndMakeCommander(varList):
    numVars = len(varList)
    if numVars == 1:
        return varList[0]
    newVarList = []
    numGrps = (numVars + maxSize-1) // maxSize
    for i in range(numGrps):
        newVarList.append(makeCommanderFromVars(varList[i*maxSize:(i+1)*maxSize]))
    return groupVarsAndMakeCommander(newVarList)


def printClausesDimacs(commentsFlag):
    if commentsFlag:
        print("c n queens completion SAT translation")
        print("c n = ", n)
        print("c num queens placed = ", (len(numbers)// 2))
        print("c ")
        print("c Placed Queens:", numbers[1:])
        print("c ")
        print("c Variable to queen Translation")
        for i in range(len(var2queen)):
            print("c Pete ",i+1," ",var2queen[i+1][0]," ",var2queen[i+1][1])
    print("p cnf ",numSatVars," ",len(clauses))
    for clause in clauses:
        for lit in clause:
            print(lit,end=" ")
        print(0)


# do some work 

readqueens()
processqueens()
clauses=[]

outrows=(list(filter(lambda a: len(a) > 0,  outrows)))
outcols=(list(filter(lambda a: len(a) > 0,  outcols)))
outd1=(list(filter(lambda a: len(a) > 1,  outd1)))
outd2=(list(filter(lambda a: len(a) > 1,  outd2)))

for file in outrows+outcols:
    fileVar=groupVarsAndMakeCommander(file)
    clauses.append([fileVar])

for diag in outd1+outd2:
    groupVarsAndMakeCommander(diag)
    # not maximally efficient as don't always need commander variable 
    # always create variable equivalent to diag but don't use that var

printClausesDimacs(outputComments)

sys.exit(0)

