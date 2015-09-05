#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Convert JSON to dzn
#
# Reads JSON from standard input and writes dzn to standard output.

import itertools
import json
import sys

# Convert a JSON object to a string.
def instance_to_dzn(obj):
    return "\n".join([ (str(k) + " = " + str(value_to_dzn(obj[k])) + ";") for k in obj.keys() ])

# Convert a JSON value (the part attached to a key) to a string.
def value_to_dzn(val):
    if type(val) is int:
        return str(val)
    if type(val) is list:
        ds = getDimensions(val)
        if not checkDimensions(val, ds):
            raise TypeError
        xs = flatten(val)
        strs = [ value_to_dzn(x) for x in xs ]
        return "".join([ "array", str(len(ds)), "d(",
                         ", ".join(["1.." + str(d) for d in ds]),
                         ", ",
                         "[", ",".join(strs), "]",
                         ")"]);
    if type(val) is str:
        return '"' + val + '"'
    if type(val) is dict:
        if "set" in val:
            strs = [ value_to_dzn(x) for x in val["set"] ]
            return "{" + ",".join(strs) + "}"

# Get the dimensions of a nested list.  Only looks at the first
# element at each depth.
def getDimensions(l):
    ds = []
    ds.append(len(l));
    x = l
    while len(x) > 0 and type(x[0]) is list:
        d = len(x[0])
        ds.append(d)
        x = x[0]
    return ds

# Check that nested lists match a certain dimensionality, and are
# rectangular (not ragged).
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

# Get the input and parse it, then convert to a dzn string.
input = "".join(sys.stdin.readlines())
obj = json.loads(input)
print(instance_to_dzn(obj))
