#!/usr/bin/env python3

import random, sys, os
from collections import namedtuple

PNAMES = ['numinstances', 'planksmin', 'planksmax',
          'lenmin', 'lenmax', 'thickmin', 'thickmax',
          'vgapmin', 'vgapmax']

Params = namedtuple('Params',PNAMES)

arg_count_ok = len(sys.argv) == 1+len(PNAMES)
if ("-h" in sys.argv) or ("--help" in sys.argv) or not arg_count_ok:
    print("USAGE:  " + sys.argv[0] + " " + " ".join(PNAMES))
    exit()

template = """language ESSENCE' 1.0
letting thickness = {thick}
letting vertical_gap = {vgap}
letting maxwidth = {maxw}
letting minshelves = 0
letting lengths = [{lengths}]
"""

p = Params(*map(int,sys.argv[1:]))

print(p)

for i in range(p.numinstances):
    nplanks = random.randrange(p.planksmin,p.planksmax)
    print("Chosen %d planks" % nplanks)
    maxplanklen_hehe = random.randrange(int((p.lenmin+p.lenmax)/2),p.lenmax)
    plengths = [random.randrange(p.lenmin,maxplanklen_hehe) for _ in range(nplanks)]
    thick = random.randrange(p.thickmin,p.thickmax)
    vgap = random.randrange(p.vgapmin,p.vgapmax)
    maxw = random.choice([0,0,0,random.randrange(min(plengths)*2)])
    lengths = ",".join([str(x) for x in plengths])
    fname = "planks_{:03d}_{:02d}.param".format(i,nplanks)
    fbody = template.format(
        thick=thick,
        vgap=vgap,
        maxw=maxw,
        lengths=lengths
        )
    with open(os.path.join("instances",fname),"wt") as f:
        f.write(fbody)
