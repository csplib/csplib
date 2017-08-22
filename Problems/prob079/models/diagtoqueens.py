#!/usr/bin/env python3

# program to convert from queens and ruled out diagonals to report stats and convert to dimacs

import sys


def An(n):
    assert( n % 6 == 1 or n % 6 == 5)
    res = []
    for i in range(n):
        res.append([i,(2*i)%n])
    return( res)

def Bn(n):
    assert( n % 6 == 1 or n % 6 == 5)
    res = []
    for i in range(n):
        res.append([(2*i)%n,i])
    return( res)

def timesn(n,q): 
    return [q[0]*n,q[1]*n] 

def plusqueen(q1,q2):
    return [q1[0]+q2[0],q1[1]+q2[1]]

def offsetqueens(n,offsetq,queens):
    return [plusqueen(timesn(n,offsetq),q) for q in queens]

q19col0 = [0,8]
q19col19 = [19,8]
q19row0 = [17,0]
q19row19 = [17,19]
q19rest = [[1,12],[2,17],[3,7],[4,10],[5,15],[6,1],[7,16],[8,13],[9,2],[10,6],[11,18],[12,3],[13,5],[14,11],[15,9],[16,14],[18,4]]

def max(numbers):
    assert(len(numbers) != 0)
    max=numbers[0]
    for i in range(1,len(numbers)):
        if numbers[i] > max:
            max=numbers[i]
    return max

def checkrowcols(Queens):
    if(Queens == []):
        return True
    n1 = len(Queens)
    n2 = max([q[0] for q in Queens] + [q[1] for q in Queens]) + 1
    if( n1 > n2):
        return False
    rowbits = [0 for i in range(n2)]
    colbits = [0 for i in range(n2)]
    for q in Queens:
        colbits[q[0]] = 1
        rowbits[q[1]] = 1
    return ( (len([1 for i in range(n2) if rowbits[i] != 0]) == n1) and 
             (len([1 for i in range(n2) if colbits[i] != 0]) == n1) )

def checkdiags(Queens):
    if(Queens == []):
        return True
    n1 = len(Queens)
    n2 = max([q[0] for q in Queens] + [q[1] for q in Queens]) + 1
    if( n1 > n2):
        return False
    d1bits = [0 for i in range(2*n2)]
    d2bits = [0 for i in range(2*n2)]
    for q in Queens:
        d1bits[q[0]+q[1]] = 1
        d2bits[q[0]-q[1]+n2-1] = 1
    return ( (len([1 for i in range(2*n2) if d1bits[i] != 0]) == n1) and 
             (len([1 for i in range(2*n2) if d2bits[i] != 0]) == n1) )

def checknoattack(Queens):
    return checkrowcols(Queens) and checkdiags(Queens)

### e.g. reduce3to2(7,[],[],[])

def reduce3to2(M3):
    n3=M3[0]
    P3=M3[1]
    m3=M3[2]
    C3=M3[3]
    R3=M3[4]
    assert len(C3) == len(R3), ("different numbers of rows and columns: ")
    assert m3 == len(C3)+len(P3), "m inconsistent with length of cols and preplaced queens"
    assert m3 <= n3, ("more queens to place than places to put them: " + str(m3) + " " + str(n3))
    if P3 or C3 or R3:
        ncheck = max([q[0] for q in P3] + [q[1] for q in P3] + C3 + R3) 
        assert ncheck < n3, ("Row or column required more than input value of n")
    assert(checkrowcols(P3)), ("preplaced queens have two in same row or column: " + str(P3))
    Cprime = C3 + [q[0] for q in P3]
    Rprime = R3 + [q[1] for q in P3]
    assert len(set(Rprime)) == m3, "preplaced queen in one of required rows"
    assert len(set(Cprime)) == m3, "preplaced queen in one of required cols"
    if not checkdiags(P3):
        return [n3,P3]
    if n3%3 == 2:
        nprime = n3*2+1
    else:
        nprime = n3*2-1
    an = An(nprime)
    bn = Bn(nprime)
    qscol0 = [q for q in offsetqueens(nprime,q19col0,an) if q[0] not in Cprime]
    qsrow0 = [q for q in offsetqueens(nprime,q19row0,bn) if q[1] not in Rprime]
    qsrest = qscol0+qsrow0
    for qr in q19rest:
        qsrest += [q for q in offsetqueens(nprime,qr,an)]
    Cprime.sort()
    Rprime.sort()
    QC = [[i,2*Cprime[i]] for i in range(m3)]
    QR = [[2*Rprime[i],i] for i in range(m3)]
    result = P3
    result += offsetqueens(nprime,q19col19,QC)
    result += offsetqueens(nprime,q19row19,QR)
    result += qsrest
    assert(checknoattack(result)),("Bug: computed attacking set of queens: "+str(result))
    return [19*nprime+m3,result]


def reduce4to3(n4,m4,C4,R4,D4diff,D4sum):
    n3=n4*7-3
    m3=m4+len(D4diff)+len(D4sum)
    Qdiff = [ [6*n4-3-d,3*n4-1-2*d] for d in D4diff ]
    Qsum =  [ [2*n4-2-d,n4+2*d] for d in D4sum ]
    C3=[c+3*n4-2 for c in C4]
    R3=R4
    return [n3,Qdiff+Qsum,m3,C3,R3]

# Special case where all rows and columns allowed
def reducesimple4to3(M4Simple):
    n = M4Simple[0]
    Ddiff = M4Simple[1]
    Dsum = M4Simple[2]
    return reduce4to3(n,n,list(range(n)),list(range(n)),Ddiff,Dsum)

# Numbers as read from input file 
# numbers[0] = n
# numbers[1] = maxdiags
# numbers[2,4,6 ... ] = diagonal, +n-1 for difference diagonals
# numbers[3,5,7... ] = 1 for sum diag, 0 for diff

def diagtosimple4(numdiags,numbers):
    n = numbers[0]
    maxdiags = numbers[1]
    assert numdiags <= maxdiags, "more diagonals requested than available in input"
    assert len(numbers) >= numdiags*2 + 2, "fewer diagonals than claimed"
    Ddiff=[]
    Dsum=[]
    for i in range(numdiags):
        d=numbers[i*2+2]
        direction=numbers[i*2+2+1]
        assert(direction == 0 or direction == 1)
        if direction == 1: 
            Dsum.append(d)
        else: 
            Ddiff.append(d-n+1)
    return [n,Ddiff,Dsum]
    


def reducesimple4to2(M4Simple):
    return reduce3to2(reducesimple4to3(M4Simple))

def printqueens2(M2):
    print("letting n = ",M2[0])
    print("letting init = ",M2[1])

def printqueensfromdiag(numdiags,numbers):
    printqueens2(reducesimple4to2(diagtosimple4(numdiags,numbers)))
