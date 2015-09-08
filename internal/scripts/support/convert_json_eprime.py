#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Convert JSON to E' parameter format

import itertools
import json
import sys

input = "".join(sys.stdin.readlines())
obj = json.loads(input)

def instance_to_eprime(obj):
    return "\n".join([ ("letting " + str(k) + " = " + str(value_to_eprime(obj[k]))) for k in obj.keys() ])

def value_to_eprime(val):
    if type(val) is int:
        return str(val)
    if type(val) is list:
        ds = getDimensions(val)
        if not checkDimensions(val, ds):
            raise TypeError
        strs = [ value_to_eprime(x) for x in val ]
        return "["+(",".join(strs))+"]"
    if type(val) is str:
        return '"' + val + '"'
    if type(val) is dict:
        if "set" in val:
            strs = [ value_to_eprime(x) for x in val["set"] ]
            return "toSet([" + ",".join(strs) + "])"

def getDimensions(l):
    ds = []
    ds.append(len(l));
    x = l
    while len(x) > 0 and type(x[0]) is list:
        d = len(x[0])
        ds.append(d)
        x = x[0]
    return ds

def checkDimensions(l, ds):
    d = ds[0]
    if len(l) != d:
        return False
    if len(ds) == 1:
        return True
    inners = [ checkDimensions(li, ds[1:]) for li in l ]
    result = True
    for i in inners:
        result = result and i
    return result

# Stolen from http://stackoverflow.com/a/5409395
flatten = lambda *n: (e for a in n
    for e in (flatten(*a) if isinstance(a, (tuple, list)) else (a,)))

print(instance_to_eprime(obj))
