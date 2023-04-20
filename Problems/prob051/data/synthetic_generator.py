# coding=utf-8
import random

def genInstance(nCargo, nTanks, nEmptyTanks,
                tankMinSize, tankMaxSize, tankFillRatioMin, tankFillRatioMax,
                connectivityRatio, impossibleCargoRatio, notNeighbouringRatio):
    # Let's generate the tanker graph
    nearbyTanks = [[] for i in range(0, nTanks)]

    allConnections = [(a, b) for a in range(0, nTanks) for b in range(a + 1, nTanks)]
    random.shuffle(allConnections)

    print("---")
    print(len(allConnections))
    print(connectivityRatio)
    print(int(len(allConnections) * connectivityRatio))
    for i in range(0, int(len(allConnections)*connectivityRatio)):
        nearbyTanks[allConnections[i][0]].append(allConnections[i][1])
        nearbyTanks[allConnections[i][1]].append(allConnections[i][0])

    # Generate a base assignment for the cargo
    allCargo = list(range(0, nCargo))
    tankAssign = [random.choice(allCargo) for i in range(0, nTanks)]
    tankSize = [random.randint(tankMinSize, tankMaxSize) for i in range(0, nTanks)]

    # Select "empty" tanks
    for i in range(0, nEmptyTanks):
        tankAssign[random.randint(0, nTanks-1)] = -1

    # Generate the effective load of each cargo
    cargoLoad = [0 for i in range(0, nCargo)]
    for i in range(0, nTanks):
        if tankAssign[i] != -1:
            cargoLoad[tankAssign[i]] += int(tankSize[i]*random.uniform(tankFillRatioMin, tankFillRatioMax))

    # For each tank, generate a random list of impossible choices
    impossibleCargo = [[] for i in range(0, nTanks)]
    for i in range(0, nTanks):
        impossibleCargo[i] = [j for j in range(0, nCargo) if j != tankAssign[i] and random.random() <= impossibleCargoRatio]

    # Generate list of all neighbouring cargos
    neighbouringCargos = set()
    for i in range(0, nTanks):
        for j in nearbyTanks[i]:
            if tankAssign[i] != -1 and tankAssign[j] != -1:
                neighbouringCargos.add((tankAssign[i], tankAssign[j]))
                neighbouringCargos.add((tankAssign[j], tankAssign[i]))
    nonNeighbouringCargos = [(m, n) for m in range(0, nCargo) for n in range(m+1, nCargo)
                             if (m, n) not in neighbouringCargos and random.random() <= notNeighbouringRatio]

    problem = """<?xml version="1.0" encoding="UTF-8"?>
<!--
This is a random problem, generated with these parameters:
nCargo = {}
nTanks = {}
nEmptyTanks = {}
tankMinSize = {}
tankMaxSize = {}
tankFillRatioMin = {}
tankFillRatioMax = {}
connectivityRatio = {}
impossibleCargoRatio = {}
notNeighbouringRatio = {}
-->
""".format(nCargo, nTanks, nEmptyTanks,
              tankMinSize, tankMaxSize, tankFillRatioMin, tankFillRatioMax,
              connectivityRatio, impossibleCargoRatio, notNeighbouringRatio)
    problem += """<problem>\n<cargos nb="{}">\n""".format(nCargo)
    for i in range(0, nCargo):
        problem += """\t<cargo id="{}" name="{}" volume="{}"/>\n""".format(i+1, "c"+str(i+1), cargoLoad[i])
    problem += """</cargos>\n"""
    problem += """<incompatibles>\n"""
    for a,b in nonNeighbouringCargos:
        problem += """\t<incompatible cargo1="{}"  cargo2="{}"/>\n""".format(a+1, b+1)
    problem += """</incompatibles>\n<tanks nb="{}">\n""".format(nTanks)

    for i in range(0, nTanks):
        problem += """\t<tank id="{}" capa="{}">\n""".format(i+1, tankSize[i])
        problem += """\t\t<impossiblecargos>\n"""
        for j in impossibleCargo[i]:
            problem += """\t\t\t<cargo id="{}"/>\n""".format(j+1)
        problem += """\t\t</impossiblecargos>\n"""
        problem += """\t\t<neighbours>\n"""
        for j in nearbyTanks[i]:
            problem += """\t\t\t<tank id="{}"/>\n""".format(j+1)
        problem += """\t\t</neighbours>\n"""
        problem += """\t</tank>\n"""

    problem += """</tanks>\n"""
    problem += """</problem>\n"""

    return problem


nCargoList = [15, 25, 30]
nTanksList = [25, 35, 40]
nEmptyTanksList = [1]

tankSizes = [(200, 1000), (200, 300)]
tankFillRatios = [(0.9,1.0),(0.5,1.0)]

connectivityRatioList = [0.25, 0.5, 0.75]
impossibleCargoRatioList = [0.25, 0.5, 0.75]
notNeighbouringRatioList = [0.25, 0.5, 0.75]

nRepeats = 3
idx = 0
for nCargo in nCargoList:
    for nTanks in nTanksList:
        for nEmptyTanks in nEmptyTanksList:
            if nCargo < nTanks + nEmptyTanks:
                for (tankMinSize, tankMaxSize) in tankSizes:
                    for (tankFillRatioMin, tankFillRatioMax) in tankFillRatios:
                        for connectivityRatio in connectivityRatioList:
                            for impossibleCargoRatio in impossibleCargoRatioList:
                                for notNeighbouringRatio in notNeighbouringRatioList:
                                    for i in range(0, nRepeats):
                                        f = open("instances/chemTanker_{}.xml".format(idx), 'w')
                                        f.write(genInstance(nCargo, nTanks, nEmptyTanks,
                                                            tankMinSize, tankMaxSize, tankFillRatioMin, tankFillRatioMax,
                                                            connectivityRatio, impossibleCargoRatio, notNeighbouringRatio))
                                        f.close()
                                        idx += 1
