#!/usr/bin/env python3

# program to simply rule out certain numbers of diags 

import sys
import random
import re

def mean(l): 
    return float(sum(l)) / max(len(l), 1)

# numbers = [int(s) for s in re.findall('\d+', sys.stdin.read())]

if len(sys.argv) < 4: 
     print("Usage:", sys.argv[0],end="")
     print("  n mininfile seed")
     sys.exit(0)

n = int(sys.argv[1])
mininfile = int(sys.argv[2])
random.seed(int(sys.argv[3]))

outputComments = True

# setup data structures
rows = [ 0 for i in range(n) ]
cols = [ 0 for i in range(n) ]
numinrow = [ n for i in range(n) ]
numincol = [ n for i in range(n) ]
d1 = [ 0 for i in range(2*n-1) ]
d2 = [ 0 for i in range(2*n-1) ]
diags1 = [i for i in range(2*n-1)]
diags2 = [i for i in range(2*n-1)]
random.shuffle(diags1)
random.shuffle(diags2)

outdiags = [] 


# direction = +1 or -1. +1 for sum  diagonal and -1 for difference
# sum diag d = r+c.  So c = d-r  
# diff diag d = r-c+n-1. So c = r-d+n-1

def updatediag(d,direction): 
    global d1
    global d2
    global outdiags
    assert(direction == 1 or direction == -1)
    killedrows = []
    killedcols = []
    for row in range(n):
        if direction == 1: 
            col = d-row
        else: 
            col = row-d+n-1
        if col in range(n):
            if noattack([row,col]):
                if numinrow[row] <= mininfile or numincol[col] <= mininfile: 
                    return False
                killedrows.append(row)
                killedcols.append(col)
    #print(d," ",direction," ", killedrows, killedcols)
    #not returned FALSE so actually do it
    for row in killedrows :
        numinrow[row] -= 1
    for col in killedcols :
        numincol[col] -= 1
    if direction == +1:
        d1[d] = 1
        outdiags.append([d,1])
    else:
        d2[d] = 1
        outdiags.append([d,0])
    return True

def generatediags():
    global diags1
    global diags2
    numsofar = 0
    while((diags1 or diags2) and (numsofar < numdiags)):
        direction=random.randrange(-1,2,2) # i.e. -1 or +1
        if direction== 1 and diags1:    # i.e. there is a diagonal to use 
            d=diags1.pop()
        else:                           # use other diagonal
            direction=-1
            d=diags2.pop()
        if updatediag(d,direction):
            numsofar += 1
    return(numsofar == numdiags)

def generatediags_all():
    global diags1
    global diags2
    numsofar = 0
    while((diags1 or diags2)):
        if not diags1: 
            direction= -1
        elif not diags2:
            direction= 1
        else:
            direction=random.randrange(-1,2,2) # i.e. -1 or +1
        if direction== 1:    # i.e. using sum diagonal
            d=diags1.pop()
        else:                           # use other diagonal
            d=diags2.pop()
        if updatediag(d,direction):
            numsofar += 1


numSatVars = 0

def readqueens():
    numargs = int(len(numbers)-1)
    for i in range(int(numargs/2)):
        queenx=int(numbers[i*2+1])
        queeny=int(numbers[i*2+2])
        if noattack([queenx,queeniy]):
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
    global openrows
    global opencols
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
        print("c num diags ruled out = ", numdiags)
        print("c ")
        #print("c Placed Queens:", numbers[1:])
        print("c ")
        print("c Variable to queen Translation")
        for i in range(len(var2queen)):
            print("c   ",i+1,": ",var2queen[i+1])
    print("p cnf ",numSatVars," ",len(clauses))
    for clause in clauses:
        for lit in clause:
            print(lit,end=" ")
        print(0)


# do some work 

#readqueens()
generatediags_all()

print("n = ",n)
print("numdiags = ", len(outdiags))
print("diags = ", outdiags)


sys.exit(0)

processqueens()

clauses=[]

outd1=(list(filter(lambda a: len(a) > 1,  outd1)))
outd2=(list(filter(lambda a: len(a) > 1,  outd2)))

if False: 
    rowlens = [len(outrows[row]) for row in openrows]
    collens = [len(outcols[row]) for row in opencols]
    print(min(rowlens),mean(rowlens),max(rowlens))
    print(min(collens),mean(collens),max(collens))
    outd1=(list(filter(lambda a: len(a) > 0,  outd1)))
    outd2=(list(filter(lambda a: len(a) > 0,  outd2)))
    collens = [len(d1) for d1 in outd1]
    print(min(collens),mean(collens),max(collens),len(collens))
    collens = [len(d1) for d1 in outd2]
    print(min(collens),mean(collens),max(collens),len(collens))
    outd1=(list(filter(lambda a: len(a) > 1,  outd1)))
    outd2=(list(filter(lambda a: len(a) > 1,  outd2)))
    collens = [len(d1) for d1 in outd1]
    print(min(collens),mean(collens),max(collens),len(collens))
    collens = [len(d1) for d1 in outd2]
    print(min(collens),mean(collens),max(collens),len(collens))


for file in outrows+outcols:
    fileVar=groupVarsAndMakeCommander(file)
    clauses.append([fileVar])

for diag in outd1+outd2:
    groupVarsAndMakeCommander(diag)
    # not maximally efficient as don't always need commander variable 
    # always create variable equivalent to diag but don't use that var

printClausesDimacs(outputComments)
sys.exit(0)


