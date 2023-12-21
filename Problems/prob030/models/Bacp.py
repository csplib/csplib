"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python Bacp.py -data=Bacp_10.json -variant=m1
  python Bacp.py -data=Bacp_10.json -variant=m2
  python Bacp.py -data=Bacp_10.json -variant=m1-d
  python Bacp.py -data=Bacp_10.json -variant=m2-d
"""

from pycsp3 import *

nCourses, nPeriods, minCredits, maxCredits, minCourses, maxCourses, credits, prerequisites = data
maxCredits = maxCredits * maxCourses if subvariant("d") else maxCredits
assert nCourses == len(credits)

# s[c] is the period (schedule) for course c
s = VarArray(size=nCourses, dom=range(nPeriods))

# co[p] is the number of courses at period p
co = VarArray(size=nPeriods, dom=range(minCourses, maxCourses + 1))

# cr[p] is the number of credits at period p
cr = VarArray(size=nPeriods, dom=range(minCredits, maxCredits + 1))

if variant("m1"):
    def table(c):
        return {(0,) * p + (credits[c],) + (0,) * (nPeriods - p - 1) + (p,) for p in range(nPeriods)}


    # cp[c][p] is 0 if the course c is not planned at period p, the number of credits for c otherwise
    cp = VarArray(size=[nCourses, nPeriods], dom=lambda c, p: {0, credits[c]})

    satisfy(
        # channeling between arrays cp and s
        [(*cp[c], s[c]) in table(c) for c in range(nCourses)],

        # counting the number of courses in each period
        [Count(s, value=p) == co[p] for p in range(nPeriods)],

        # counting the number of credits in each period
        [Sum(cp[:, p]) == cr[p] for p in range(nPeriods)]
    )

elif variant("m2"):
    # pc[p][c] is 1 iff the course c is at period p
    pc = VarArray(size=[nPeriods, nCourses], dom={0, 1})

    satisfy(
        # tag(channeling)
        [iff(pc[p][c], s[c] == p) for p in range(nPeriods) for c in range(nCourses)],

        # ensuring that each course is assigned to a period
        [Sum(pc[:, c]) == 1 for c in range(nCourses)],

        # counting the number of courses in each period
        [Sum(pc[p]) == co[p] for p in range(nPeriods)],

        # counting the number of credits in each period
        [pc[p] * credits == cr[p] for p in range(nPeriods)]
    )

satisfy(
    # handling prerequisites
    s[c1] < s[c2] for (c1, c2) in prerequisites
)

if subvariant("d"):
    minimize(
        # minimizing the maximal distance in term of credits
        Maximum(cr) - Minimum(cr)
    )
else:
    minimize(
        # minimizing the maximum number of credits in periods
        Maximum(cr)
    )

annotate(decision=s)
