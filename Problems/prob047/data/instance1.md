---
Title: (4 agents, 2 tiers)
---

#### Note on instance description

This instance is based on a supply chain topology referred to as the 'W' system, which is composed of 2 final products produced from 3 components. Each product is assembled from a common and a unique component. This system has the characteristics of pure distribution (common component is used in both products) and pure assembly structure. W system is well-known in the assemble-to-order inventory literature.

The instance description consists of two parts:

1.  Topology: the supply chain network topology consists of (i) a list of agents; (ii) a list of products that each agent produces (*agent-name=product-name(s)*); and (iii) a bill of materials specifying what products are used in building other products (*product-name=component-name(s)*).
2.  Local problems: the private data describing each agent's local problem - this is in the form of an OPL data file and is for use in conjunction with the OPL models provided in this benchmark description.

#### Topology

    Agents: A1,A2,A3,A4

    Products: 
    A1=P0,P1
    A2=P2
    A3=P3
    A4=P4

    BOM:
    P0=P2,P3
    P1=P3,P4

#### Agent: A1

    numPeriods = 12;
    numProducts = 2;
    numComponents = 3;
    maxNumComponentOrders = [30,30,30];
    bom = [
    [1,1,0],
    [0,1,1]
    ];
    componentBatchSize = [90,100,100];
    deliveryCost = [4050,3200,3000];
    leadtime = [0,1];
    cycles = [2,1];
    setupCycles = [7,5];
    capacity = [183,149,139,134,157,113,139,161,189,158,138,113];
    demand = [
    [41,34],
    [55,36],
    [10,30],
    [64,40],
    [44,70],
    [54,64],
    [24,22],
    [21,50],
    [47,54],
    [49,24],
    [50,18],
    [53,45]
    ];
    penaltyCost = [317,268];
    openingProductInventory = [0,98];
    openingComponentInventory = [71,80,128];
    productHoldingCost = [33,22];
    componentHoldingCost = [15,14,9];
    setupCosts = [3300,2200];

#### Agent: A2

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [90];
    leadtime = [1];
    cycles = [1];
    setupCycles = [7];
    capacity = [68,60,66,74,67,66,67,64,65,57,63,60];
    openingProductInventory = [81];
    openingComponentInventory = [66];
    productHoldingCost = [15];
    componentHoldingCost = [17];
    setupCosts = [1500];
    componentArrivals = [
    [63],
    [69],
    [54],
    [59],
    [55],
    [59],
    [51],
    [52],
    [69],
    [62],
    [65],
    [70]
    ];

#### Agent: A3

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [100];
    leadtime = [1];
    cycles = [1];
    setupCycles = [6];
    capacity = [97,89,99,85,94,77,77,87,86,92,87,97];
    openingProductInventory = [87];
    openingComponentInventory = [69];
    productHoldingCost = [14];
    componentHoldingCost = [14];
    setupCosts = [1400];
    componentArrivals = [
    [92],
    [88],
    [91],
    [92],
    [84],
    [75],
    [86],
    [97],
    [90],
    [75],
    [100],
    [81]
    ];

#### Agent: A4

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [100];
    leadtime = [2];
    cycles = [1];
    setupCycles = [9];
    capacity = [67,67,67,75,56,57,52,69,55,50,75,50];
    openingProductInventory = [61];
    openingComponentInventory = [54];
    productHoldingCost = [9];
    componentHoldingCost = [6];
    setupCosts = [900];
    componentArrivals = [
    [68],
    [74],
    [61],
    [53],
    [63],
    [51],
    [50],
    [73],
    [70],
    [52],
    [55],
    [52]
    ];