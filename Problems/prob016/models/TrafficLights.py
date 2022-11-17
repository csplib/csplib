"""
PyCSP3 Model (see pycsp.org)

Execution:
  python3 TrafficLights.py
"""

from pycsp3 import *

R, RY, G, Y = "red", "red-yellow", "green", "yellow"

table = {(R, R, G, G), (RY, R, Y, R), (G, G, R, R), (Y, R, RY, R)}

# v[i] is the color for the ith vehicle traffic light
v = VarArray(size=4, dom={R, RY, G, Y})

# p[i] is the color for the ith pedestrian traffic light
p = VarArray(size=4, dom={R, G})

satisfy(
    (v[i], p[i], v[(i + 1) % 4], p[(i + 1) % 4]) in table for i in range(4)
)
