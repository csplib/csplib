"""
PyCSP3 Model (see pycsp.org)

Data can come:
 - either directly from a JSON file
 - or from an intermediate parser

Examples:
  python TemplateDesign.py -data=TemplateDesign_catfood_2.json
  python TemplateDesign.py -data=TemplateDesign_catfood_2.json -variant=aux
"""

from pycsp3 import *
from math import ceil, floor

nSlots, demands = data
nTemplates = nVariations = len(demands)


def variation_interval(v):
    return range(ceil(demands[v] * 0.95), floor(demands[v] * 1.1) + 1)


# d[i][j] is the number of occurrences of the jth variation on the ith template
d = VarArray(size=[nTemplates, nVariations], dom=range(nSlots + 1))

# p[i] is the number of printings of the ith template
p = VarArray(size=nTemplates, dom=range(max(demands) + 1))

satisfy(
    # all slots of all templates are used
    Sum(d[i]) == nSlots for i in range(nTemplates)
)

if not variant():
    satisfy(
        # respecting printing bounds for each variation
        p * d[:, j] in variation_interval(j) for j in range(nVariations)
    )

elif variant("aux"):
    # pv[i][j] is the number of printings of the jth variation by using the ith template
    pv = VarArray(size=[nTemplates, nVariations], dom=lambda i, j: range(variation_interval(j).stop))

    satisfy(
        # linking variables of arrays p and pv
        [p[i] * d[i][j] == pv[i][j] for i in range(nTemplates) for j in range(nVariations)],

        # respecting printing bounds for each variation v
        [Sum(pv[:, j]) in variation_interval(j) for j in range(nVariations)]
    )

satisfy(
    # tag(symmetry-breaking)
    [
        [iff(p[i] == 0, d[i][0] == nSlots) for i in range(nTemplates)],

        Decreasing(p),
    ]
)

minimize(
    # minimizing the number of used templates
    Sum(p[i] > 0 for i in range(nTemplates))
)
