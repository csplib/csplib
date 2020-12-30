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
    componentBatchSize = [96,62,99];
    deliveryCost = [3552,1674,3960];
    leadtime = [2,0];
    cycles = [2,2];
    setupCycles = [9,6];
    capacity = [169,132,132,188,171,145,194,112,159,194,199,130];
    demand = [
    [22,50],
    [48,84],
    [97,114],
    [101,92],
    [80,33],
    [25,76],
    [25,45],
    [20,70],
    [95,56],
    [112,53],
    [111,100],
    [40,101]
    ];
    penaltyCost = [485,272];
    openingProductInventory = [50,68];
    openingComponentInventory = [33,70,50];
    productHoldingCost = [40,28];
    componentHoldingCost = [24,16,17];
    setupCosts = [4000,2800];

#### Agent: A2

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [96];
    leadtime = [0];
    cycles = [2];
    setupCycles = [6];
    capacity = [73,68,92,80,55,74,56,72,98,79,91,55];
    openingProductInventory = [46];
    openingComponentInventory = [9];
    productHoldingCost = [24];
    componentHoldingCost = [20];
    setupCosts = [2400];
    componentArrivals = [
    [33],
    [41],
    [26],
    [36],
    [44],
    [36],
    [47],
    [26],
    [48],
    [40],
    [34],
    [41]
    ];

#### Agent: A3

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [62];
    leadtime = [1];
    cycles = [1];
    setupCycles = [7];
    capacity = [82,67,78,83,89,72,68,54,61,94,83,66];
    openingProductInventory = [1];
    openingComponentInventory = [62];
    productHoldingCost = [16];
    componentHoldingCost = [20];
    setupCosts = [1600];
    componentArrivals = [
    [65],
    [56],
    [61],
    [99],
    [63],
    [84],
    [71],
    [88],
    [50],
    [67],
    [97],
    [86]
    ];

#### Agent: A4

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [99];
    leadtime = [0];
    cycles = [2];
    setupCycles = [10];
    capacity = [83,61,76,74,94,64,55,71,63,100,75,61];
    openingProductInventory = [25];
    openingComponentInventory = [2];
    productHoldingCost = [17];
    componentHoldingCost = [13];
    setupCosts = [1700];
    componentArrivals = [
    [43],
    [27],
    [28],
    [42],
    [50],
    [39],
    [48],
    [27],
    [35],
    [41],
    [49],
    [46]
    ];